resourceGenerators in Compile <+= (streams, baseDirectory in Compile, resourceManaged in Compile) map { (str, srcDir, outDir) =>
	val src = srcDir / "av.c"
	val out = outDir / "libav.so"
	if (FileInfo.lastModified(src).lastModified >= FileInfo.lastModified(out).lastModified) {
		val pkg = "pkg-config --cflags --libs libavformat" !! ;
		val jh = System.getProperty("java.home")
		// This does not handle spaces in paths properly:
		val cmd = "gcc -Wall -fPIC -shared -o " + out + " -I" + jh + "/include " + pkg.trim + " " + src
		str.log.info(cmd)
		if ((cmd !) != 0)
			throw (new MessageOnlyException(src + ": Error"))
	}
	Seq(out)
}
