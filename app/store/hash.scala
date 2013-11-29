package store

object Hex {
  private final val digits = "0123456789abcdef".toCharArray
  def apply(b : Array[Byte]) = {
    val a = new Array[Char](b.length << 1)
    (0 until b.length) foreach { i =>
      a(i << 1) = digits((b(i) >>> 4) & 0xf)
      a((i << 1) + 1) = digits(b(i) & 0xf)
    }
    new String(a)
  }
}

sealed abstract class Hash(name : String) {
  import java.security.MessageDigest
  private[this] def getDigest : MessageDigest = MessageDigest.getInstance(name)
  private[this] final val BUFSIZE = 32768
  def apply(file : java.io.File) : Array[Byte] = {
    val in = new java.io.FileInputStream(file)
    try {
      val digest = getDigest
      val buf = new Array[Byte](BUFSIZE)
      @scala.annotation.tailrec def loop() {
        val n = in.read(buf, 0, BUFSIZE)
        if (n > -1) {
          digest.update(buf, 0, n)
          loop()
        }
      }
      loop()
      digest.digest
    } finally {
      in.close
    }
  }
  def apply(b : Array[Byte]) : Array[Byte] =
    getDigest.digest(b)
  def apply(s : String) : Array[Byte] =
    apply(s.getBytes)
  def hex(b : Array[Byte]) =
    Hex(apply(b))
  def hex(s : String) =
    Hex(apply(s))
}

object MD5 extends Hash("MD5")
object SHA1 extends Hash("SHA-1")
