package su.moi.lerrox.jgraphTest

import org.jgrapht.alg._
import org.jgrapht.graph._
import org.jgrapht.traverse.TopologicalOrderIterator
import scala.collection.JavaConversions._


/**
 * Created by lerrox on 13.07.15.
 */
object GraphtTest extends App{

  val mapString = scala.io.Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt").mkString
  val mountainsByCoordinates =   mapString.split("\n").zipWithIndex.flatMap{
    case(line, lineNum) =>
      line.split("\\W+").zipWithIndex.map{
        case (valueString, columnNum) =>
          Item(Point(lineNum, columnNum), valueString.toInt)
      }
  }.groupBy(item => item.coordinates).mapValues(_.head)

  val graph = new DefaultDirectedGraph[Item, DefaultWeightedEdge](classOf[DefaultWeightedEdge])

  println("Adding nodes")
  mountainsByCoordinates.values.foreach(v => graph.addVertex(v))
  println(s"Added ${graph.vertexSet().size()} nodes")
  val shifts:Array[(Int, Int)] = Array((-1,0), (1,0), (0,-1), (0,1))

  println("Adding edges")
  graph.vertexSet().foreach{
    v =>
      val point = v.coordinates
      shifts.foreach{
        sh =>
          val maybeMountain = mountainsByCoordinates.get(Point(point.x+sh._1, point.y+sh._2))
          maybeMountain match {
            case Some(item) => {
              if(v.hight > item.hight){
                val e = graph.addEdge(v, item)
                val weight = v.hight - item.hight
                graph.setEdgeWeight(e, weight * -1) //cheating
               // println(s"create Graph edje ((${v.coordinates.x}, ${v.coordinates.y}})->(${item.coordinates.x},${item.coordinates.y}})) with weight ${weight}")
              }
            }

            case None => 
          }
      }
  }
  println("edges added")

  val iter = new TopologicalOrderIterator(graph)

  val maxPathCost:Option[MaxCost] = iter.foldLeft[Option[MaxCost]](None){
    (old, item) =>
    if(!old.isEmpty && old.get.cost < (item.hight * -1))  old
    else {
      val magicPathFinder = new BellmanFordShortestPath(graph, item)
      val max = graph.vertexSet().foldLeft[Option[MaxCost]](None){
        (currentOld, nextItem) =>
          val dif = item.hight - nextItem.hight
          if(nextItem == item) old
          else {
            val cost:Double = magicPathFinder.getCost(nextItem)
            currentOld match {
              case Some(currentMax) => {
                if(currentMax.cost > cost){
                  Some(new MaxCost(item, nextItem, cost))
                } else {
                  Some(currentMax)
                }
              }
              case None => Some(new MaxCost(item, nextItem, cost))
            }
          }
      }

      old match {
        case None => max
        case Some(oldMax) => {
          max.map{
            m =>
              if(m.cost >= oldMax.cost) oldMax
              else {
                println(s"max cost changed to ${m.cost} ")
                m
              }
          }
        }
      }
    }

  }

  maxPathCost match {
    case None => println("not found an answer")
    case Some(max) =>{
      val magicPathFinder = new BellmanFordShortestPath(graph, max.from)
      val longestPath = magicPathFinder.getPathEdgeList(max.to)
      val pathNodesCount = longestPath.size() + 1

      println(s"longest path hight is ${max.cost * -1}, and it's path through ${pathNodesCount} mountains")
    }
  }

}


case class Item(coordinates:Point, hight:Int)
case class Point(x:Int, y:Int)
case class MaxCost(from:Item, to:Item, cost:Double)
