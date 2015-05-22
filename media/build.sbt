libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.4"
)

resourceGenerators in Compile <+= (streams, baseDirectory in Compile, resourceManaged in Compile) map { (str, srcDir, outDir) =>
	val src = (PathFinder(srcDir) * "*.c").get
	val out = outDir / System.mapLibraryName("media")
	val outmod = FileInfo.lastModified(out).lastModified
	if (src.exists(FileInfo.lastModified(_).lastModified >= outmod)) {
		val slash = java.io.File.separator
		val pkg = try {
			"pkg-config --cflags --libs libavformat libswscale libavcodec libavutil".!!
		} catch {
			case _ : java.lang.Exception => "-I/usr/local/lib -L/usr/local/lib -lavformat -lavcodec -lavutil"
		}
		val jh = Option(System.getenv("JAVA_HOME")).getOrElse(System.getProperty("java.home") + slash + "..") + slash + "include"
		// This does not handle spaces in paths properly:
		val cmd = "gcc -Wall -fPIC -shared -o " + out + " -I" + jh + " -I" + jh + "/linux " + src.mkString(" ") + " " + pkg.trim + " -lcrack"
		str.log.info(cmd)
		outDir.mkdirs
		if (cmd.! != 0)
			throw (new MessageOnlyException(src.mkString(" ") + ": Error"))
	}
	Seq(out)
}
