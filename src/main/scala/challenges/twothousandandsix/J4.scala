package challenges.twothousandandsix

import utils.io

import scala.util.matching.Regex


object J4 extends App {

  case class EncryptedRoom(name: String, sector: Int, checksum: String) {
    override def toString: String = s"$name | $sector | $checksum"
  }

  def parser(s: String): Option[EncryptedRoom] = {
    val roomRegex: Regex = """([a-z-]+)-([\d]+)\[(\w*)\]""".r
    s match {
      case roomRegex(encryptedName, sector, checksum) => Some(EncryptedRoom(encryptedName, sector.toInt, checksum))
      case _ => None
    }
  }

  def isValid(encryptedRoom: EncryptedRoom): Boolean = {
    val nameWithoutDash = encryptedRoom.name.filter(_ != '-')
    val orderedByOccurrenceLetters = nameWithoutDash
      .groupBy(identity)
      .mapValues(_.length)
      .toSeq
      .sortWith {
        case ((c1: Char, occ1: Int), (c2: Char, occ2: Int)) => {
          if (occ1 > occ2) true
          else if (occ1 == occ2) c1 < c2
          else false
        }
      }
      .map(_._1)
      .take(5)
      .mkString
    orderedByOccurrenceLetters == encryptedRoom.checksum
  }

  val source: Seq[Option[EncryptedRoom]] = io.readLines("2016/j4input.txt").map(parser).toSeq
  val test = Seq(
    "b-c-d-e-f-g-h-a-b-c-c-a-a-b-987[abcde]",
    "aaaaa-bbb-z-y-x-123[abxyz]",
    "not-a-real-room-404[oarel]",
    "totally-real-room-200[decoy]"
  ).map(parser).toSeq
  val input = source

  val roomList: Option[Seq[EncryptedRoom]] = if (input.exists(_.isEmpty)) None else Some(input.map(_.get).toList)

  val validRoomsList: Option[Seq[EncryptedRoom]] = roomList.map(_.filter(isValid))

  validRoomsList.foreach {
    rl => println(rl.foldLeft(0)((acc, e) => acc + e.sector))
  }

  val clearedNameRoomsList = validRoomsList.map {
    vrl =>
      vrl.map {
        r => {
          val clearName = r.name.map {
            case '-' => ' '
            case c => (((c - 97 + r.sector) % 26) + 97).toChar
          }
          (clearName, r.sector)
        }
      }
  }

  clearedNameRoomsList.foreach(_.filter(_._1.contains("north")).foreach(println))
}
