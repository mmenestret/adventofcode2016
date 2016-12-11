package challenges.twothousandandsix

import utils.io

import scala.util.matching.Regex

object J9 extends App {

  val source = io.readLines("2016/j9input.txt").toList.head
  val markerRegex: Regex = """\((\d+)x(\d+)\)""".r

  def repeat(cs: String, times: Int): String = Seq.fill[String](times)(cs).flatten.mkString

  def uncompress(input: String,
                 markerBuffer: Seq[Char],
                 outputBuffer: StringBuilder): String = {
    markerBuffer.mkString match {
      case markerRegex(sequenceLength, times) =>
        outputBuffer.append(repeat(input.take(sequenceLength.toInt), times.toInt)).mkString
//      case m if !m.isEmpty =>
    }
  }

  println(uncompress("blablobli", "(3x2)".toCharArray, StringBuilder.newBuilder.append("test")))
}