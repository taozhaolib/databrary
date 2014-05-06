import sbt._
import sbt.Keys._
import play.PlayExceptions.AssetCompilationException

object JSConcatCompiler extends play.PlayAssetsCompiler with Plugin {
  val directory = SettingKey[String]("js-concat-directory")
  val entryPoints = SettingKey[PathFinder]("js-concat-file")
  val externs = SettingKey[Seq[URL]]("js-concat-externs")

  // needed for "broken" https on github
  System.setProperty("jsse.enableSNIExtension", "false")

  import com.google.javascript.jscomp.{Compiler,CompilerOptions,JSSourceFile,CompilationLevel}

  private def concat(src : Seq[JSSourceFile]) =
    src.flatMap { s =>
      Seq("/* " + file(s.getName).getName + " */", s.getCode)
    }.mkString("\n")

  private def compile(externs : Seq[URL], cacheDir : File)(file : File, options : Seq[String]) : (String, Option[String], Seq[File]) = {
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
        case "ecmascript5_strict" => o.setLanguageIn(CompilerOptions.LanguageMode.ECMASCRIPT5_STRICT)
        case _ => Unit // Unknown option
      })
      o
    }

    val ext = externs.map { ext =>
      val cache = new File(cacheDir, new File(ext.getPath).getName)
      if (!cache.exists) {
	cache.getParentFile.mkdirs
	try {
	  val out = new java.io.FileOutputStream(cache)
	  sbt.BasicIO.transferFully(ext.openStream, out)
	  out.close
	} catch { case e : Exception =>
	  cache.delete
	  throw e
	}
      }
      JSSourceFile.fromFile(cache)
    }
    val extsrc = concat(ext)

    val rest = PathFinder(file.getParentFile).descendantsExcept("*.js", "_*").get diff Seq(file)
    val all = (file +: rest).map(JSSourceFile.fromFile(_))

    val compiler = new Compiler()
    try {
      if (!compiler.compile(Array[JSSourceFile](), all.toArray, opts).success) {
        val e = compiler.getErrors().head
        throw AssetCompilationException(Some(new File(e.sourceName)), e.description, Some(e.lineNumber), None)
      }
      (extsrc + concat(all), Some(extsrc + compiler.toSource), rest)
    } catch {
      case e : Exception =>
        e.printStackTrace()
        throw AssetCompilationException(Some(file), "Internal Closure Compiler error (see logs)", None, None)
    }
  }

  val Compiler = Def.bind(cacheDirectory zip externs) { case (cacheDir, ext) =>
    AssetsCompiler("javascripts",
      (_ / "assets" ** "*.js"),
      entryPoints,
      { (name, min) => if (min) name.replace(".js", ".min.js") else name },
      compile(ext, cacheDir),
      play.Keys.closureCompilerOptions)
  }

  override val projectSettings = Seq(
    directory := "javascripts",
    entryPoints <<= (sourceDirectory in Compile, directory){ (base, dir) =>
      base / "assets" / dir / "app.js"
    },
    externs := Nil
  )
}
