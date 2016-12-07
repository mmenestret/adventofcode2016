package challenges

import utils.io

object J7 extends App {

  val source = io.readLines("j7input.txt").toList
  val test = List(
    "abba[mnop]qrst",
    "abcd[bddb]xyyx",
    "aaaa[qwer]tyui",
    "ioxxoj[asdfgh]zxcvbn"
  )
  val firstIpChunksRegex = """(\w+)\[""".r
  val lastIpChunksRegex = """\w+$""".r
  val squareChunkRegex = """\[(\w+)\]""".r

  case class Ip(superNetSeq: Seq[String], squareSeq: Seq[String]) {
    override def toString: String = s"${superNetSeq.mkString("-")} | ${squareSeq.mkString("-")}"
  }

  def parser(s: String): Ip = {
    val firstsSuperNetChuncksList: List[String] = firstIpChunksRegex.findAllMatchIn(s).map(_.group(1)).toList
    val lastSuperNetChuncksList: String = lastIpChunksRegex.findFirstIn(s).get
    val squareChunckList: List[String] = squareChunkRegex.findAllMatchIn(s).map(_.group(1)).toList

    Ip(firstsSuperNetChuncksList :+ lastSuperNetChuncksList, squareChunckList)
  }

  def containsABBA(s: String): Boolean = {

    val slided: Seq[String] = s.sliding(2).toList

    def helper(l: Seq[String]): Boolean = {
      l match {
        // head != third is to check if we are not in the case of the same two letters (aa and aa)
        case head :: sec :: third :: rest if head != third => head == third.reverse || helper(l.tail)
        case head :: sec :: Nil => false
        case Nil => false
        case _ => helper(l.tail)
      }
    }

    helper(slided)
  }

  def retrieveABAs(s: String): Option[Seq[String]] = {
    val slided: Seq[String] = s.sliding(3).toList
    val res = slided.filter(chunck => chunck(0) == chunck(2))
    if (res.isEmpty) None else Some(res)
  }

  def abaToBab(s: String): String = Seq(s(1), s(0), s(1)).mkString

  def containsBAB(s: String, aba: String): Boolean = {
    s.sliding(3).toList.contains(abaToBab(aba))
  }

  val ips = source.map(parser)

  val nbOfGoodIp = ips.count {
    ip => {
      val abbaInCore = ip.superNetSeq.exists(containsABBA)
      val abbaInSquares = ip.squareSeq.exists(containsABBA)
      abbaInCore && !abbaInSquares
    }
  }

  val ipsWithaba = ips.filter {
    ip => ip.superNetSeq.exists(retrieveABAs(_).nonEmpty)
  }

  val abasWithBabs = ipsWithaba.count {
    ipWithaba => {
      val abas = ipWithaba.superNetSeq.flatMap(retrieveABAs).flatten
      abas.exists {
        aba => ipWithaba.squareSeq.exists(containsBAB(_, aba))
      }
    }
  }

  println(abasWithBabs)
}