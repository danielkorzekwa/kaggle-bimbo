package bimbo.data

import java.io.File
import scala.io.Source
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.text.SimpleDateFormat
import java.util.TimeZone
import org.joda.time.LocalDate
import java.util.Date

case class CSVBimboTestItemDS( inputFile: String) {

  def getAllItems(): Seq[Item] = {

    val items: Seq[Item] = Source.fromFile(new File(inputFile)).getLines().drop(1).map { l =>
      val item: Item = createItem(l)
      item
    }.toList

    items
  }

  private def createItem(l: String): Item = {
    val lArray = l.split(",")
    
    val productId = lArray(6).toInt
    val demand = Double.NaN
    val item = Item(productId,demand)
    item
  }
}