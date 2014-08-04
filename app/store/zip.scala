package store

import scala.collection.mutable
import scala.concurrent.Future
import play.api.libs.iteratee._
import org.databrary.iteratee.ZipFile
import macros._
import site._
import models._

object Zip {
  implicit val executionContext = context.process

  private def comment(obj : SiteObject) : String =
    Site.url(obj.pageURL)

  private def enum[A](a : A, e2 : Future[Enumerator[A]]) : Enumerator[A] =
    Enumerator(a) >>> Enumerator.flatten(e2)

  private def slotAssets(slot : Slot, prefix : String) : Future[Seq[ZipFile.DeflatedStreamEntry]] =
    slot.assets.map { assets =>
      val names = mutable.Set.empty[String]
      assets.filter(_.checkPermission(Permission.READ)).map { sa =>
	val base = sa.asset.name.getOrElse(sa.asset.format.name)
	val ext = "." + sa.format.extension
	var name = base + ext
	var i = 1
	while (!names.add(name)) {
	  i += 1
	  name = base + i + ext
	}
	new ZipFile.DeflatedStreamEntry(prefix + name,
	  Enumerator.flatten(Asset.read(sa)),
	  Asset.timestamp(sa),
	  comment = comment(sa))
      }
    }

  private def slotName(slot : Slot, names : scala.collection.Set[String] = Set.empty[String]) : Future[String] =
    slot.fileName.map { base =>
      var name = base
      var i = 1
      while (names.contains(name)) {
	i += 1
	name = base + i
      }
      name
    }

  private def zip(obj : SiteObject)(entries : Future[Enumerator[ZipFile.StreamEntry]]) : Enumerator[Array[Byte]] =
    Enumerator.flatten(entries) &>
      ZipFile.flatZip(comment = comment(obj), level = java.util.zip.Deflater.BEST_SPEED)

  private def slotEntries(slot : Slot, prefix : String, names : scala.collection.Set[String] = Set.empty[String]) : Future[(String, Enumerator[ZipFile.StreamEntry])] =
    slotName(slot, names).map { name =>
      name ->
      enum(new ZipFile.DirEntry(prefix + name, comment = comment(slot)),
	slotAssets(slot, prefix + Maybe(name).fold("")(_ + "/")).map(Enumerator(_ : _*)))
    }

  def slot(slot : Slot) : Enumerator[Array[Byte]] = zip(slot) {
    slot.volume.fileName.map { vname =>
      enum(new ZipFile.DirEntry(vname, comment = comment(slot.volume)),
	slotEntries(slot, vname + "/").map(_._2))
    }
  }

  def volume(vol : Volume) = zip(vol) {
    vol.fileName.map { vname =>
      enum(new ZipFile.DirEntry(vname, comment = comment(vol)),
	vol.containers.map { slots =>
	  val prefix = vname + "/"
	  val names = mutable.Set.empty[String]
	  Enumerator(slots : _*) &> Enumeratee.mapFlatten[Container] { slot =>
	    Enumerator.flatten {
	      for {
		(name, ents) <- slotEntries(slot, prefix, names)
	      } yield {
		names += name
		ents
	      }
	    }
	  }
	}
      )
    }
  }
}
