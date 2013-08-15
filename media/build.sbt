resourceGenerators in Compile <+= (streams, baseDirectory in Compile, resourceManaged in Compile) map { (str, srcDir, outDir) =>
	val src = srcDir / "av.c"
	val out = outDir / System.mapLibraryName("av")
	if (FileInfo.lastModified(src).lastModified >= FileInfo.lastModified(out).lastModified) {
		val pkg = try {
			"pkg-config --cflags --libs libavformat".!!
		} catch {
			case e : java.io.IOException => "-I/usr/local/lib -L/usr/local/lib -lavformat -lavcodec -lavutil"
		}
		val jh = System.getProperty("java.home")
		// This does not handle spaces in paths properly:
		val cmd = "gcc -Wall -fPIC -shared -o " + out + " -I" + jh + "/include " + pkg.trim + " " + src
		str.log.info(cmd)
		outDir.mkdirs
		if (cmd.! != 0)
			throw (new MessageOnlyException(src + ": Error"))
	}
	Seq(out)
}
