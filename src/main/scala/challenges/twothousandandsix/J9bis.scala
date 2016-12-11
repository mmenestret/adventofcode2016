package challenges.twothousandandsix

import utils.io

import scala.util.parsing.combinator.RegexParsers

case class Marker(lenght: Int, times: Int)

sealed trait uncompressedMessageBlock extends Product with Serializable

case class transformedBlock(nbOfRepetition: Int, sequence: String) extends uncompressedMessageBlock

case class normalBlock(sequence: Char) extends uncompressedMessageBlock

object parser extends RegexParsers {

  def marker = '(' ~> ("\\d+".r <~ 'x') ~ "\\d+".r <~ ')' ^^ {
    case sequenceLenght ~ nbOfRepetition =>
      Marker(sequenceLenght.toInt, nbOfRepetition.toInt)
  }

  def markedSequence = marker.flatMap {
    case Marker(sequenceLenght, nbOfRepetition) =>
      repN(sequenceLenght, ".".r) ^^ { sequence => transformedBlock(nbOfRepetition, sequence.mkString) }
  }

  def normalSequence = ".".r ^^ { c => {
    assert(c.length == 1)
    normalBlock(c.head)}}

  val expr = rep(markedSequence | normalSequence)

  def apply(input: String) = parseAll(expr, input)
}

object J9bis extends App {

  val source = io.readLines("2016/j9input.txt").toList.head

  val input = "(10x3)(1x2)WKQANMDILI(2x7)Q(1x6)OOWQZDNGORPHFNHBKKKVQEJNUVNAQ"

  val uncompressedFile: List[uncompressedMessageBlock] = parser(source).get

  val lenght = uncompressedFile.map{
    case transformedBlock(t, s) => t * s.length
    case _ => 1
  }.sum

  println(lenght)
}