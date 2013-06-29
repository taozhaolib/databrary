package models

import anorm._

final class Orcid private (val orcid : String) extends scala.collection.immutable.WrappedString(orcid)
{
  def valid : Boolean = lengthCompare(16) == 0 && {
    val (b, cs) = orcid.splitAt(15)
    val d = b.map(_.asDigit)
    d.forall(i => i >= 0 && i < 10) && {
      val c = 10 - ((d.foldLeft(0)(2*_ + 2*_) + 9) % 11)
      cs.head == (if (c == 10) 'X' else '0' + c)
    }
  }

  override def toString : String = orcid.grouped(4).mkString("-")
  def uri : String = "http://orcid.org/" + toString
}

object Orcid {
  def apply(s : String) : Orcid =
    new Orcid(s.filterNot(c => c == '-' || c.isSpaceChar).stripPrefix("http://").stripPrefix("orcid.org/"))

  implicit val typeMapper = scala.slick.lifted.MappedTypeMapper.base[Orcid, String](_.orcid, new Orcid(_))

  implicit val rowToOrcid : Column[Orcid] = Column(Column.rowToString(_, _).map(new Orcid(_)))
}
