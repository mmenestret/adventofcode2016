package challenges.twothousandandsix

import java.security.MessageDigest

import scala.collection.immutable.Stream

object J5 extends App {

  def byteToHexString(a: Byte): String = "%02x".format(a)
  def byteArrayToHexString(a: Array[Byte]): String = a.map(byteToHexString).mkString

  def md5(s: String): Array[Byte] = {
    MessageDigest
      .getInstance("MD5")
      .digest(s.getBytes)
  }

  // if the 5 first bytes == 0 and the 6th represents an int between 0 and 7
  def matchTheConditions(a: Array[Byte]): Boolean = {
    (a(0) & 0xFF) == 0 &&
      (a(1) & 0xFF) == 0 &&
      (a(2) & 0xF0) == 0 &&
      (a(2) & 0x0F) <= 7
  }

  def collectPasswordCharacters(acc: Map[Int, Char],
                                correctMd5s: Stream[Array[Byte]]): Map[Int, Char] = {
    if (acc.size == 8) acc
    else {
      val correctMd5 = correctMd5s.head
      val position = correctMd5(2) & 0x0F
      if (acc.keys.toSeq.contains(position)) collectPasswordCharacters(acc, correctMd5s.tail)
      else {
        val newPasswordChar = byteArrayToHexString(correctMd5).charAt(6)
        val newAcc = acc + (position -> newPasswordChar)
        collectPasswordCharacters(newAcc, correctMd5s.tail)
      }
    }
  }

  val keyBase = "wtnhxymk"
  val keyStream = {
    def gen(ite: Int): Stream[String] = s"$keyBase$ite" #:: gen(ite + 1)
    gen(0)
  }

  val start = java.lang.System.currentTimeMillis()
  val correctMd5s = keyStream
    .map(md5)
    .filter(matchTheConditions)

  val passwordCharacters = collectPasswordCharacters(Map[Int, Char](), correctMd5s)

  val password = passwordCharacters
    .toSeq
    .sortBy { case ((position, char)) => position }
    .map { case (position, char) => char }
    .mkString

  val timeIsMs = (java.lang.System.currentTimeMillis - start) / 1000

  println(s"$password in $timeIsMs sec")
}