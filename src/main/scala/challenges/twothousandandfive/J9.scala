package challenges.twothousandandfive

import utils.io

object J9 extends App {

  case class Edge(from: Node, to: Node, distance: Int) { override def toString: String = s"From $from to $to: $distance" }
  case class Node(city: String) { override def toString: String = city }
  case class Graph(nodes: Set[Node], edges: Set[Edge]){
    override def toString: String = {
      s"""
         |NODES:
         |${nodes.mkString("\n")}
         |
         |EDGES:
         |${edges.toSeq.sortBy(_.from.city).mkString("\n")}
       """.stripMargin
    }
    def from(fromCity: String): Set[Edge] = edges.filter(_.from.city == fromCity)
  }

  val source = io.readLines("2015/j9input.txt").toList

  val pathRegex = """(\w+) to (\w+) = (\d+)""".r

  val graph = source.foldLeft(Graph(Set[Node](), Set[Edge]())) {
    (previousGraph, line) => {
      line match {
        case pathRegex(from: String, to: String, dist: String) =>
          val fromNode = Node(from)
          val toNode = Node(to)
          Graph(
            previousGraph.nodes + (fromNode, toNode),
            previousGraph.edges + (
              Edge(fromNode, toNode, dist.toInt),
              Edge(toNode, fromNode, dist.toInt)))
        case _ => previousGraph
      }
    }
  }

  println(graph)


}