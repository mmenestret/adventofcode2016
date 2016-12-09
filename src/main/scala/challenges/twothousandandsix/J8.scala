package challenges.twothousandandsix

import utils.io

sealed trait Instruction

case class Rect(x: Int, y: Int) extends Instruction

case class RotateR(y: Int, steps: Int) extends Instruction

case class RotateC(x: Int, steps: Int) extends Instruction

case class Screen(panel: Seq[Seq[Int]], x: Int, y: Int) {
  override def toString: String = {
    panel.map {
      r => r.map(c => if (c == 0) '.' else '#').mkString
    }.mkString("\n")
  }
}

object J8 extends App {

  val xSize = 50
  val ySize = 6

  val screen = Screen(Seq.fill[Seq[Int]](ySize)(Seq.fill[Int](xSize)(0)), xSize, ySize)

  val rectRegex = """rect (\d+)x(\d+)""".r
  val rotateRRegex = """rotate row y=(\d+) by (\d+)""".r
  val rotateCRegex = """rotate column x=(\d+) by (\d+)""".r

  def parser(s: String): Instruction = {
    s match {
      case rectRegex(x, y) => Rect(x.toInt, y.toInt)
      case rotateRRegex(y, steps) => RotateR(y.toInt, steps.toInt)
      case rotateCRegex(x, steps) => RotateC(x.toInt, steps.toInt)
    }
  }

  val source = io.readLines("2016/j8input.txt").toList
  val instructions = source.map(parser)


  def applyRect(screen: Screen, rect: Rect): Screen = {

    def overlap(s1: Screen, s2: Screen): Screen = {
      assert(s1.x == s2.x && s1.y == s2.y)
      Screen(s1.panel.zip(s2.panel).map {
        case (l1: Seq[Int], l2: Seq[Int]) => l1.zip(l2).map {
          case (i1: Int, i2: Int) => i1 + i2
        }
      }, s1.x, s1.y)
    }

    val Rect(x, y) = rect
    val rectCols = Seq.fill[Int](x)(1)
    val otherScreenCols = Seq.fill[Int](screen.x - x)(0)
    val rectRows = rectCols ++ otherScreenCols
    val otherScreenRows = Seq.fill[Seq[Int]](screen.y - y)(Seq.fill[Int](screen.x)(0))
    val rectScreen = Screen(Seq.fill[Seq[Int]](y)(rectRows) ++ otherScreenRows, screen.x, screen.y)

    overlap(screen, rectScreen)
  }

  def rotate[A, C](coll: C, number: Int)(
    implicit c2i: C => Iterable[A], cbf: collection.generic.CanBuildFrom[C, A, C]
  ): C = {
    val positions = number % coll.size match {
      case i if i < 0 => i + coll.size
      case i => i
    }
    val builder = cbf()
    builder ++= coll.drop(positions)
    builder ++= coll.take(positions)
    builder.result()
  }

  def applyRotateC(screen: Screen, rotateC: RotateC): Screen = {
    val screenTransposed = screen.panel.transpose
    val lineToRotate = screenTransposed(rotateC.x)
    val lineRotated = rotate(lineToRotate, -rotateC.steps)
    val mutablePanel = collection.mutable.Seq[Seq[Int]](screenTransposed: _*)
    mutablePanel(rotateC.x) = lineRotated
    screen.copy(mutablePanel.transpose)
  }

  def applyRotateR(screen: Screen, rotateR: RotateR): Screen = {
    val rowToRotate = screen.panel(rotateR.y)
    val lineRotated = rotate(rowToRotate, -rotateR.steps)
    val mutablePanel = collection.mutable.Seq[Seq[Int]](screen.panel: _*)
    mutablePanel(rotateR.y) = lineRotated
    screen.copy(mutablePanel)
  }

  def applyInstruction(screen: Screen, instruction: Instruction): Screen = {
    instruction match {
      case i: Rect => applyRect(screen, i)
      case i: RotateC => applyRotateC(screen, i)
      case i: RotateR => applyRotateR(screen, i)
    }
  }

  val newScreen = instructions.foldLeft(screen)(applyInstruction)
  val count = newScreen.panel
    .map{_.map{i => if(i > 0) 1 else 0}}
    .map(_.sum)
    .sum

  println(count)
  println(newScreen)
}