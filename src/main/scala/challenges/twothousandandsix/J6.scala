package challenges.twothousandandsix

import utils.io

object J6 extends App {

  val source = io.readLines("2016/j6input.txt").toList

  val newLists = (0 until source.head.length).map {
    index => source.map(_ (index)).mkString
  }

  val ordered = newLists.map {
    s =>
      s
        .groupBy(identity)
        .mapValues(_.length)
        .toSeq
        .sortBy(-_._2)
        .map(_._1)
  }.map(_.head).mkString
  println(ordered)
}