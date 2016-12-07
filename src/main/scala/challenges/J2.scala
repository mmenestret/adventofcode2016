package challenges

import utils.io


object J2 extends App {
  type Instruction = Char
  type Position = Int

  /*
   0 1 2
   3 4 5
   6 7 8
   Careful, here we retrieve numbers from 0 to 8
   If the position returned is a code button, you have to add 1 to it (since it's 1 based and not 0 based)
   */
  def nextPosition1(currentPosition: Position, instruction: Instruction): Position = {

    val leftEdges = Seq(0, 3, 6)
    val bottomEdges = Seq(6, 7, 8)
    val rightEdges = Seq(2, 5, 8)
    val topEdges = Seq(0, 1, 2)

    instruction match {
      case 'L' => if (leftEdges.contains(currentPosition)) currentPosition else currentPosition - 1
      case 'R' => if (rightEdges.contains(currentPosition)) currentPosition else currentPosition + 1
      case 'U' => if (topEdges.contains(currentPosition)) currentPosition else currentPosition - 3
      case 'D' => if (bottomEdges.contains(currentPosition)) currentPosition else currentPosition + 3
    }
  }

  /*
      0
    1 2 3
  4 5 6 7 8
    9 A B
      C
   Careful, here we retrieve numbers from 0 to 12
   If the position returned is a code button, you have to add 1 to it (since it's 1 based and not 0 based)
   And then translate 10 -> A / 11 -> B / 12 -> C / 13 -> D
   */
  def nextPosition2(currentPosition: Position, instruction: Instruction): Position = {

    val leftEdges = Seq(0, 1, 4, 9, 12)
    val bottomEdges = Seq(4, 9, 12, 11, 8)
    val rightEdges = Seq(12, 11, 8, 3, 0)
    val topEdges = Seq(8, 3, 0, 1, 4)

    instruction match {
      case 'L' => if (leftEdges.contains(currentPosition)) currentPosition else currentPosition - 1
      case 'R' => if (rightEdges.contains(currentPosition)) currentPosition else currentPosition + 1
      case 'U' =>
        if (topEdges.contains(currentPosition)) currentPosition
        else if (currentPosition == 12 || currentPosition == 2) currentPosition - 2
        else currentPosition - 4
      case 'D' =>
        if (bottomEdges.contains(currentPosition)) currentPosition
        else if (currentPosition == 0 || currentPosition == 10) currentPosition + 2
        else currentPosition + 4
    }
  }

  def nextButton(currentPosition: Position,
                 instructionList: Seq[Instruction],
                 nextPositionFunction: (Position, Instruction) => Position): Position = {
    instructionList.foldLeft(currentPosition) {
      (lastPosition, currentInstruction) => nextPositionFunction(lastPosition, currentInstruction)
    }
  }

  val input = io.readLines("j2input.txt").toList

  val code1 = input.foldLeft(List[Position]()) {
    (lastPressedButton, instructionsListForNextButton) => {
      val lastPosition = if (lastPressedButton.isEmpty) 4 else lastPressedButton.last
      lastPressedButton :+ nextButton(lastPosition, instructionsListForNextButton, nextPosition1)
    }
  }

  println(code1.map(_ + 1).mkString("-"))

  val code2 = input.foldLeft(List[Position]()) {
    (lastPressedButton, instructionsListForNextButton) => {
      val lastPosition = if (lastPressedButton.isEmpty) 4 else lastPressedButton.last
      lastPressedButton :+ nextButton(lastPosition, instructionsListForNextButton, nextPosition2)
    }
  }

  println(code2.map(_ + 1).mkString("-"))
}
