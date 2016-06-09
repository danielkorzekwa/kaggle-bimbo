package bimbo.data

import java.io.File
import scala.io.Source
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.text.SimpleDateFormat
import java.util.TimeZone
import org.joda.time.LocalDate
import java.util.Date

case class CSVBimboTestItemDS( inputFile: String) {

  def getAllItems(): Seq[TestItem] = {

    val items: Seq[TestItem] = Source.fromFile(new File(inputFile)).getLines().drop(1).map { l =>
      val item: TestItem = createItem(l)
      item
    }.toList

    items
  }

  private def createItem(l: String): TestItem = {
    val lArray = l.split(",")
    val item = TestItem()
    item
  }
}