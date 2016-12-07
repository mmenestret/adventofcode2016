package challenges

import utils.io


object J3 extends App {

  case class Shape(sides: Seq[Int]){
    override def toString: String = sides.mkString("-")
  }

  val input: List[Array[Int]] = io.readLines("j3input.txt").map{
    line =>
      line
        .trim
        .split("\\D")
        .filter(!_.isEmpty)
        .map(_.toInt)
  }.toList

  /*val shapes = input.map{
    line => Shape(line.map(_.toInt))
  }*/

  def isTriangle(shape: Shape): Boolean = {
    val maxSide = shape.sides.max
    val otherSides = shape.sides.diff(Seq(maxSide))
    assert(otherSides.size == 2)
    maxSide < otherSides.sum
  }

  /*val triangles = shapes.filter(isTriangle)

  println(triangles.size)*/

  val (s0, s1, s2) = input.foldLeft((List[Int](), List[Int](), List[Int]())){
    case ((s0, s1, s2), elem) => (s0 :+ elem(0), s1 :+ elem(1), s2 :+ elem(2))
  }

  val c = s0.grouped(3).map(Shape(_)).count(isTriangle) + s1.grouped(3).map(Shape(_)).count(isTriangle) + s2.grouped(3).map(Shape(_)).count(isTriangle)
  println(c)
}
