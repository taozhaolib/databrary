import sbt._
import sbt.Keys._
import play.PlayExceptions.AssetCompilationException

object JSConcatCompiler extends play.PlayAssetsCompiler with Plugin {
  val directory = SettingKey[String]("js-concat-directory")
  val entryPoints = SettingKey[PathFinder]("js-concat-file")

  /*
  private def readAll(files : Seq[File]) : String = {
    val out = new StringBuilder
    val buf = new Array[Char](32768)
    files.foreach { f =>
      out ++= "\n// " + f + " /\n"
      val i = new java.io.FileReader(f)
      var n
      while ((n = i.read(buf)) >= 0)
	out.appendAll(buf, 0, n)
      i.close
    }
    out.result
  }
  */

  import com.google.javascript.jscomp.{Compiler,CompilerOptions,JSSourceFile,CompilationLevel}

  private def compile(file : File, options : Seq[String]) : (String, Option[String], Seq[File]) = {
    val opts = {
      val o = new CompilerOptions()
      o.closurePass = true

      options.foreach({
        case "advancedOptimizations" => CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(o)
        case "checkCaja" => o.setCheckCaja(true)
        case "checkControlStructures" => o.setCheckControlStructures(true)
        case "checkTypes" => o.setCheckTypes(true)
        case "checkSymbols" => o.setCheckSymbols(true)
        case "ecmascript5" => o.setLanguageIn(CompilerOptions.LanguageMode.ECMASCRIPT5)
        case _ => Unit // Unknown option
      })
      o
    }

    val rest = PathFinder(file.getParentFile).descendantsExcept("*.js", "_*").get diff Seq(file)
    val all = (file +: rest).map(JSSourceFile.fromFile(_))
    val ext = Array[JSSourceFile]()

    val compiler = new Compiler()

    try {
      if (!compiler.compile(ext, all.toArray, opts).success) {
        val e = compiler.getErrors().head
        throw AssetCompilationException(Some(new File(e.sourceName)), e.description, Some(e.lineNumber), None)
      }
      (all.flatMap { s =>
	Seq("/* " + s.getName + " */", s.getCode)
      }.mkString("\n"), Some(compiler.toSource), rest)
    } catch {
      case e : Exception =>
        e.printStackTrace()
        throw AssetCompilationException(Some(file), "Internal Closure Compiler error (see logs)", None, None)
    }
  }

  val Compiler = AssetsCompiler("javascripts",
    (_ / "assets" ** "*.js"),
    entryPoints,
    { (name, min) => if (min) name.replace(".js", ".min.js") else name },
    compile,
    play.Keys.closureCompilerOptions
  )

  override val projectSettings = Seq(
    directory := "javascripts",
    entryPoints <<= (sourceDirectory in Compile, directory){ (base, dir) =>
      base / "assets" / dir / "app.js"
    }
  )
}
