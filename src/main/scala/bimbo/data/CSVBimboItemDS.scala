package bimbo.data

import java.io.File

import scala.io.Source

import com.typesafe.scalalogging.slf4j.LazyLogging

case class CSVBimboItemDS( inputFile: String) {

  def getAllItems(): Seq[Item] = {

    val items: Seq[Item] = Source.fromFile(new File(inputFile)).getLines().drop(1).map { l =>
      val item: Item = createItem(l)
      item
    }.toList

    items
  }

  private def createItem(l: String): Item = {
    val lArray = l.split(",")
    
    val demand = lArray(10).toDouble
    val item = Item(demand)
    
    item
  }
}