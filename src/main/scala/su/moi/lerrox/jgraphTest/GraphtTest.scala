package su.moi.lerrox.jgraphTest

import java.util.Date

import org.jgrapht.alg._
import org.jgrapht.graph._
import org.jgrapht.traverse.TopologicalOrderIterator
import scala.collection.JavaConversions._


/**
 * Created by lerrox on 13.07.15.
 */
object GraphtTest extends App{
  println(s"started ${new Date()}")
  val mapString = scala.io.Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt").mkString
  //val mapString = "4 4 \n4 8 7 3 \n2 5 9 3 \n6 3 2 5 \n4 4 1 6"
  val mountainsByCoordinates =   mapString.split("\n").zipWithIndex.flatMap{
    case(line, lineNum) =>
      line.trim.split(" ").zipWithIndex.map{
        case (valueString, columnNum) =>
          Item(Point(columnNum, lineNum), valueString.toInt)
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
                graph.setEdgeWeight(e, -1) //cheating
               // println(s"create Graph edje ((${v.coordinates.x}, ${v.coordinates.y}})->(${item.coordinates.x},${item.coordinates.y}})) with weight ${weight}")
              }
            }

            case None => 
          }
      }
  }
  println("edges added")

  val iter = new TopologicalOrderIterator(graph)

  var max:Option[MaxCost] = None

  iter.zipWithIndex.foreach{
    case(item, count)=>
      val magicWand = new BellmanFordShortestPath(graph, item)
      for(nextItem <- graph.vertexSet()){
        if(item.hight > nextItem.hight){
          max match {
            case Some(oldMax) =>{
              if(item != nextItem){
                val pathLength = magicWand.getCost(nextItem)
                if(!pathLength.isInfinity && oldMax.length < pathLength * -1){
                  val drop = item.hight - nextItem.hight
                  if(drop > oldMax.drop) {
                    max = Some(MaxCost(item, nextItem, drop, pathLength * -1))
                    println(max)
                  }
                }
              }
            }
            case None => {
              if(item.hight > nextItem.hight){
                val magicWand = new BellmanFordShortestPath(graph, item)
                val length = magicWand.getCost(nextItem) * -1
                val drop = item.hight - nextItem.hight
                max = Some(MaxCost(item, nextItem, drop, length))
                println(max)
              }
            }
          }
        }

      }
  }



  max match {
    case None => println("not found an answer")
    case Some(max) =>{
      println(s"longest path drop is ${max.drop}, and it's path through ${max.length + 1} mountains")
    }
  }
  println(s"ended ${new Date()}")

}


case class Item(coordinates:Point, hight:Int)
case class Point(x:Int, y:Int)
case class MaxCost(from:Item, to:Item, drop:Int, length:Double)
