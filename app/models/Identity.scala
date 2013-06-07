package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted
import collection.mutable.HashMap
import java.sql.Timestamp
import controllers.SiteRequest
import util._

class Identity(entity : Entity) {
  final override def hashCode = id
  final def equals(o : Identity) = o.id == id

  private def cache =
    IdentityCache.add(this)

  final def id = entity.id
  final def name = entity.name
  final def orcid = entity.orcid
  final def access(implicit db : Session) : Permission.Value = entity.access

  def user : Option[User] = None

  def changeEntity(name : String = name, orcid : Option[Orcid] = orcid)(implicit site : Site) =
    entity.change(name, orcid)

  final def authorizeParents(all : Boolean = false)(implicit db : Session) = Authorize.getParents(id, all)
  final def authorizeChildren(all : Boolean = false)(implicit db : Session) = Authorize.getChildren(id, all)
}

class User(entity : Entity, account : Account) extends Identity(entity) {
  final override def user = Some(this)

  final def username = account.username
  final def email = account.email
  final def openid = account.openid

  def changeAccount(email : String = email, openid : Option[String] = openid)(implicit site : Site) =
    account.change(email, openid)
}

private object IdentityCache extends HashMap[Int, Identity] {
  def add[I <: Identity](i : I) : I = {
    update(i.id, i)
    i
  }
  add(Identity.Nobody)
  add(Identity.Root)
}

/* really a view, until slick has a better solution */
object Identity extends Table[Identity]("identity") {
  /* from entity */
  def id = column[Int]("id")
  def name = column[String]("name")
  def orcid = column[Option[Orcid]]("orcid")

  /* from account */
  def username = column[String]("username")
  def created = column[Timestamp]("created")
  def email = column[String]("email")
  def openid = column[Option[String]]("openid")
  
  def * = id ~ name ~ orcid ~ username.? ~ email.? ~ openid <> (apply _, unapply _)
  def user_* = id ~ name ~ orcid ~ username ~ email ~ openid <> (User.apply _, User.unapply _)

  def apply(id : Int, name : String, orcid : Option[Orcid], username : Option[String], email : Option[String], openid : Option[String]) = id match {
    case Entity.NOBODY => Nobody
    case Entity.ROOT => Root
    case _ => {
      val e = Entity(id, name, orcid)
      username.fold(new Identity(e))(u => new User(e, Account(id, u, email.get, openid)))
    }
  }

  def unapply(i : Identity) = {
    val u = i.user
    Some((i.id, i.name, i.orcid, u.map(_.username), u.map(_.email), u.flatMap(_.openid)))
  }

  def byId(i : Int) = Query(this).where(_.id === i)

  def byName(n : String) = {
    // should clearly be improved and/or indexed
    val w = "%" + n.split("\\s+").filter(!_.isEmpty).mkString("%") + "%"
    Query(this).filter(i => i.username === n || DBFunctions.ilike(i.name, w))
  }

  def get(i : Int)(implicit db : Session) : Identity =
    IdentityCache.getOrElseUpdate(i, 
      byId(i).firstOption.orNull)

  def create(n : String)(implicit db : Session) : Identity =
    new Identity(Entity.create(n)).cache

  final val Nobody = new Identity(Entity.Nobody)
  final val Root   = new Identity(Entity.Root)
}

object User {
  def apply(id : Int, name : String, orcid : Option[Orcid], username : String, email : String, openid : Option[String]) = 
    new User(Entity(id, name, orcid), Account(id, username, email, openid))
  def unapply(u : User) =
    Some((u.id, u.name, u.orcid, u.username, u.email, u.openid))

  def byUsername(u : String) = Query(Identity).filter(_.username === u).map(_.user_*)

  def get(i : Int)(implicit db : Session) : Option[User] = Identity.get(i).user
  def getUsername(u : String)(implicit db : Session) : Option[User] = 
    byUsername(u).firstOption
  def getOpenid(o : String, u : Option[String] = None)(implicit db : Session) : Option[User] = {
    val q = Query(Identity).filter(_.openid === o)
    u.fold(q)(u => q.filter(_.username === u)).map(_.user_*).firstOption
  }
}
