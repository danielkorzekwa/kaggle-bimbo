package bimbo.data

import java.io.File

import scala.io.Source

import com.typesafe.scalalogging.slf4j.LazyLogging

case class CSVBimboItemDS( inputFile: String) extends ItemDS{

   def getDSFile():String = inputFile
  
  def getAllItems(): Seq[Item] = {

    val items: Seq[Item] = Source.fromFile(new File(inputFile)).getLines().drop(1).map { l =>
      val item: Item = createItem(l)
      item
    }.toList

    items
  }

  private def createItem(l: String): Item = {
    val lArray = l.split(",")
    
    val weekId = lArray(0).toInt
    val depotId = lArray(1).toInt
    val channelId =  lArray(2).toInt
    val routeId = lArray(3).toInt
    val clientId = lArray(4).toInt
    val productId = lArray(5).toInt
    val demand = lArray(10).toDouble
    
    val item = Item(weekId,depotId,channelId,routeId,clientId,productId,demand)
    
    item
  }
}