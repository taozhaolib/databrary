import sbt._
import Keys._
import scala.util.matching.Regex

object AngularTemplate extends play.PlayAssetsCompiler with Plugin {
  val directory = SettingKey[String]("angular-template-directory")
  val entryPoints = SettingKey[PathFinder]("angular-template-entry-points")
  val options = SettingKey[Seq[String]]("angular-template-options")

  private def read(file : File) : String = {
    val i = new java.io.FileInputStream(file)
    val buf = new Array[Byte](i.available)
    i.read(buf)
    i.close
    new String(buf)
  }

  private final case class Replacer(source : String, regex : Regex) {
    val matches = regex.findAllMatchIn(source).toSeq
    def apply(f : Regex.Match => String) : String = {
      val b = new StringBuilder
      var i = 0
      matches.foreach { m =>
	b ++= source.substring(i, m.start)
	b ++= f(m)
	i = m.end
      }
      b ++= source.substring(i, source.length)
      b.toString
    }
  }

  private trait AllTemplater {
    def apply(l : Array[File]) : String
  }

  private abstract class AllTemplate(pre : String, post : String) extends AllTemplater {
    def each(f : File) : String
    final def apply(l : Array[File]) =
      (pre +: l.map(each) :+ post).mkString("\n")
  }
  private final class AllSubstTemplate(pre : String = "", each : String, post : String = "") extends AllTemplate(pre, post) {
    val replacer = Replacer(each, AllSubstTemplate.eachRegex)
    def each(f : File) : String =
      replacer(_.matched match {
	case "@FILENAME@" => f.getName
	case "@CONTENTS@" => read(f)
      })
  }
  private object AllSubstTemplate {
    private final val templateRegex = "(?ms)(.*?)(?:^\\{\\{\\{$(.*?))?(?:^\\}\\}\\}$(.*))?".r
    private final val eachRegex = "@FILENAME@|@CONTENTS@".r

    def apply(t : String) =
      t match {
	case templateRegex(pre, each, post) =>
	  new AllSubstTemplate(pre, Option(each).getOrElse(""), Option(post).getOrElse(""))
      }
  }

  private def compile(file : File, options : Seq[String]) : (String, Option[String], Seq[File]) = {
    if (file.getName.equals("_all.html")) {
      val all = file.getParentFile.listFiles(new java.io.FilenameFilter {
	def accept(dir : File, name : String) = name.endsWith(".html") && !name.startsWith("_")
      })
      (AllSubstTemplate(read(file))(all), None, all)
    } else
      (read(file), None, Nil)
  }

  val Compiler = AssetsCompiler("angular",
    (_ / "assets" ** "*.html"),
    entryPoints,
    { (name, min) => if (min) name.replace(".html", ".min.html") else name },
    compile,
    options
  )

  override val projectSettings = Seq(
    directory := "ng-templates",
    entryPoints <<= (sourceDirectory in Compile, directory){ (base, dir) =>
      base / "assets" / dir ** "*.html"
    },
    options := Nil
  )
}
