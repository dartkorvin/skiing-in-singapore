package su.moi.lerrox.jgraphTest

import java.util
import java.util.Date
import org.jgrapht.alg._
import org.jgrapht.graph._
import org.jgrapht.traverse.TopologicalOrderIterator
import scala.collection.JavaConversions._


/**
 * Created by lerrox on 13.07.15.
 */
object main extends App {

  val mapString = scala.io.Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt").mkString
  val guide = new MountainGuide(mapString)


  guide.longestPathWithMaxDrop() match {
    case None => println("not found an answer")
    case Some(max) => {
      println(s"longest path drop is ${max.drop}, and it's path through ${max.length} mountains")
    }
  }
  println(s"ended ${new Date()}")

}

class MountainGuide(mountainsMap: String) {

  val graph = generateGraph(mountainsMap)
  
  def longestPathWithMaxDrop(): Option[BestWay] = { // TODO not functional style, refactor it
    var max: BestWay = null
    val queue = new util.LinkedList[Item]()
    var topoList = new TopologicalOrderIterator(graph, queue).toList

    topoList.zipWithIndex.foreach{
      case (item, i)=>
        val magicWand = new BellmanFordShortestPath(graph, item)
        topoList.slice(i+1, graph.vertexSet().size()).foreach{
          nextItem =>
            val drop = item.hight - nextItem.hight
            val pathLength = magicWand.getCost(nextItem)
            if (!pathLength.isInfinity) {
              val pathLonger = max != null && max.length < (pathLength * -1 +1).toInt
              val pathEqualsButMountainHigher = max != null && max.length == (pathLength * -1 +1).toInt && drop > max.drop
              val firstValue = max == null

              if (pathLonger || pathEqualsButMountainHigher || firstValue) {
                max = BestWay(item, nextItem, drop, (pathLength * -1 +1).toInt)
                println(s"${max} : ${new Date()}")
              }
            }
        }
    }

    Option(max)
  }

  private def generateGraph(mapString: String): DefaultDirectedGraph[Item, DefaultWeightedEdge] = {
    val map = generateMap(mapString)
    val createdGraph = new DefaultDirectedGraph[Item, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    addVertexes(map, createdGraph)
    addEges(map, createdGraph)
    createdGraph
  }

  
  private def generateMap(mapString: String): Map[Point, Item] = {
    mapString.split("\n").zipWithIndex.flatMap {
      case (line, lineNum) =>
        line.trim.split(" ").zipWithIndex.map {
          case (valueString, columnNum) =>
            Item(Point(columnNum, lineNum), valueString.toInt)
        }
    }.groupBy(item => item.coordinates).mapValues(_.head)
  }

  private def addVertexes(map: Map[Point, Item], createdGraph: DefaultDirectedGraph[Item, DefaultWeightedEdge]): Unit = {
    println("Adding nodes")
    map.values.foreach(v => createdGraph.addVertex(v))
    println(s"Added ${createdGraph.vertexSet().size()} nodes")
  }

  private def addEges(map: Map[Point, Item], createdGraph: DefaultDirectedGraph[Item, DefaultWeightedEdge]): Unit = {
    val shifts: Array[(Int, Int)] = Array((-1, 0), (1, 0), (0, -1), (0, 1))
    println("Adding edges")
    createdGraph.vertexSet().foreach {
      v =>
        val point = v.coordinates
        shifts.foreach {
          sh =>
            val maybeMountain = map.get(Point(point.x + sh._1, point.y + sh._2))
            maybeMountain match {
              case Some(item) => {
                if (v.hight > item.hight) {
                  val e = createdGraph.addEdge(v, item)
                  val weight = v.hight - item.hight
                  createdGraph.setEdgeWeight(e, -1) //cheating
                  // println(s"create Graph edje ((${v.coordinates.x}, ${v.coordinates.y}})->(${item.coordinates.x},${item.coordinates.y}})) with weight ${weight}")
                }
              }
              case None =>
            }
        }
    }
    println("edges added")
  }


}


case class Item(coordinates: Point, hight: Int)

case class Point(x: Int, y: Int)

case class BestWay(from: Item, to: Item, drop: Int, length: Int)
