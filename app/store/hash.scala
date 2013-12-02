package store

sealed abstract class Encoding(name : String) {
  def apply(b : Array[Byte]) : Array[Char]
  def encodedLength(l : Int) : Int
  val decodedLength : PartialFunction[Int, Int]
}

object Hex extends Encoding("hex") {
  private final val digits = "0123456789abcdef".toCharArray
  def apply(b : Array[Byte]) = {
    val a = new Array[Char](2*b.length)
    (0 until b.length) foreach { i =>
      a(2*i  ) = digits(b(i)>>>4 & 0xf)
      a(2*i+1) = digits(b(i)     & 0xf)
    }
    a
  }
  def encodedLength(l : Int) = 2*l
  val decodedLength : PartialFunction[Int, Int] =
    { case l if l%2 == 0 => l/2 }
}

/** base64url encoding as per RFC 4648. */
object Base64 extends Encoding("base64url") {
  private final val digits = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".toCharArray
  def apply(b : Array[Byte]) = {
    val a = new Array[Char]((2+4*b.length)/3)
    (0 until b.length/3) foreach { i =>
      a(4*i  ) = digits(b(3*i)>>>2 & 0x3f)
      a(4*i+1) = digits((b(3*i)&0x3)<<4 | b(3*i+1)>>>4 & 0xf)
      a(4*i+2) = digits((b(3*i+1)&0xf)<<2 | b(3*i+2)>>>6 & 0x3)
      a(4*i+3) = digits(b(3*i+2) & 0x3f)
    }
    val i = b.length/3
    (b.length % 3 : @scala.annotation.switch) match {
      case 0 =>
      case 1 =>
        a(4*i  ) = digits(b(3*i)>>>2 & 0x3f)
        a(4*i+1) = digits((b(3*i)&0x3)<<4)
      case 2 =>
        a(4*i  ) = digits(b(3*i)>>>2 & 0x3f)
        a(4*i+1) = digits((b(3*i)&0x3)<<4 | b(3*i+1)>>>4 & 0xf)
        a(4*i+2) = digits((b(3*i+1)&0xf)<<2)
    }
    a
  }
  def encodedLength(l : Int) = (2+4*l)/3
  val decodedLength : PartialFunction[Int, Int] =
    { case l if l%4 != 1 => (3*l)/4 }
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
    new String(Hex(apply(s)))
}

object MD5 extends Hash("MD5")
object SHA1 extends Hash("SHA-1")
