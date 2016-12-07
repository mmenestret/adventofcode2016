package challenges

import utils.io

object Side extends Enumeration {
  type Side = Value
  val L, R = Value
}

object Direction extends Enumeration {
  type Direction = Value
  val N, W, S, E = Value
}


case class Position(direction: Direction.Value, x: Int, y: Int)

object J1 extends App {

  case class Instruction(direction: Side.Value, steps: Int) {
    override def toString = s"$direction -> $steps"
  }

  val input = io.readLines("j1input.txt").next()

  def parser(s: String): Option[Instruction] = {

    assert(s.length >= 2)
    // First char is the direction, rest of the string is the # of steps
    val (dir, steps) = s.splitAt(1)
    val direction = dir match {
      case "L" => Some(Side.L)
      case "R" => Some(Side.R)
      case _ => None
    }
    direction.map(Instruction(_, steps.toInt))
  }

  val parsedInput = input
    .split(", ")
    .map(parser)

  // Turn a Array[Option] is an Option[Array]
  val instructionList: Option[Array[Instruction]] = if (parsedInput.exists(_.isEmpty)) None else Some(parsedInput.map(_.get))

  // Takes a position, a instruction and add to a location list every locations by which we have to go to get to the final instruction's location
  def positionsVisited(position: Position, instruction: Instruction): Array[(Int, Int)] = {

    def recur(steps: Int, direction: Direction.Value, visited: Array[(Int, Int)]): Array[(Int, Int)] = {
      if (steps == 0) visited else {
        val lastPos = visited.last
        val newVisited = direction match {
          case Direction.W => (lastPos._1 - 1, lastPos._2)
          case Direction.S => (lastPos._1, lastPos._2 - 1)
          case Direction.E => (lastPos._1 + 1, lastPos._2)
          case Direction.N => (lastPos._1, lastPos._2 + 1)
        }

        recur(steps - 1, direction, visited :+ newVisited)
      }
    }

    val newDir = whereToHead(position, instruction)

    // We don't take the first one which is a needed starting point but which is already saved by the last move call to the function
    recur(instruction.steps, newDir, Array(position.x -> position.y)).tail
  }

  // Give the direction to head (since for example if you head south and are asked to go left,
  // it is the same than if you head south and are asked to go right
  def whereToHead(pos: Position, instruction: Instruction): Direction.Value = {
    if ((pos.direction == Direction.N && instruction.direction == Side.L) ||
      (pos.direction == Direction.S && instruction.direction == Side.R))
      Direction.W

    else if ((pos.direction == Direction.N && instruction.direction == Side.R) ||
      (pos.direction == Direction.S && instruction.direction == Side.L))
      Direction.E

    else if ((pos.direction == Direction.W && instruction.direction == Side.L) ||
      (pos.direction == Direction.E && instruction.direction == Side.R))
      Direction.S

    else Direction.N
  }

  // Compute the new position giving a starting point and an instruction
  def newPosition(pos: Position, instruction: Instruction): Position = {
    val newDir = whereToHead(pos, instruction)
    newDir match {
      case Direction.W => Position(newDir, pos.x - instruction.steps, pos.y)
      case Direction.S => Position(newDir, pos.x, pos.y - instruction.steps)
      case Direction.E => Position(newDir, pos.x + instruction.steps, pos.y)
      case Direction.N => Position(newDir, pos.x, pos.y + instruction.steps)
    }
  }

  val finalPosition = instructionList.map {
    _.foldLeft((Position(Direction.N, 0, 0), Array((0, 0)))) {
      case ((pos: Position, visitedList: Array[(Int, Int)]), instruction: Instruction) =>
        val newVisited = positionsVisited(pos, instruction)
        (newPosition(pos, instruction), visitedList ++ newVisited)
    }
  }

  def manDistanceFromBeggining(p: Position): Int = p.x.abs + p.y.abs

  finalPosition.foreach {
    p => println(manDistanceFromBeggining(p._1))
  }
  finalPosition.foreach(
    p => {
      val firstAlreadyVisited = p._2.find { e1 =>
        p._2.count {
          e2 => e1 == e2
        } >= 2
      }
      firstAlreadyVisited.foreach{
        e => println(manDistanceFromBeggining(Position(Direction.N, e._1, e._2)))
      }
    }
  )
}
