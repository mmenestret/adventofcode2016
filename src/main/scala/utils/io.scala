package utils

import scala.io.Source
import scala.util.Try

/**
  * Created by martin on 02/12/2016.
  */
object io {

  def readLines(fileName: String): Iterator[String] = Source.fromURL(getClass.getResource(s"/$fileName")).getLines()

}
