resourceGenerators in Compile <+= (streams, baseDirectory in Compile, resourceManaged in Compile) map { (str, srcDir, outDir) =>
	val src = srcDir / "av.c"
	val out = outDir / System.mapLibraryName("av")
	if (FileInfo.lastModified(src).lastModified >= FileInfo.lastModified(out).lastModified) {
		val slash = java.io.File.separator
		val pkg = try {
			"pkg-config --cflags --libs libavformat libswscale".!!
		} catch {
			case _ : java.lang.Exception => "-I/usr/local/lib -L/usr/local/lib -lavformat -lavcodec -lavutil"
		}
		val jh = Option(System.getenv("JAVA_HOME")).getOrElse(System.getProperty("java.home") + slash + "..") + slash + "include"
		// This does not handle spaces in paths properly:
		val cmd = "gcc -Wall -fPIC -shared -o " + out + " -I" + jh + " -I" + jh + "/linux " + src + " " + pkg.trim
		str.log.info(cmd)
		outDir.mkdirs
		if (cmd.! != 0)
			throw (new MessageOnlyException(src + ": Error"))
	}
	Seq(out)
}
