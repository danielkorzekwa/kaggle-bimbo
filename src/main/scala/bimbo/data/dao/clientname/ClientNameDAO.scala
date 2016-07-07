package bimbo.data.dao.clientname

import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable
import scala.collection.mutable
import scala.io.Source

case class ClientNameDAO(clientNameFile: String) {

  /**
   * key - clientId, value - clientNameId
   */
  def getClientNameIdMap(): immutable.Map[Int, Int] = {

    val i = new AtomicInteger()
    //key - clientName, value - clientNameId
    val idMap: mutable.Map[String, Int] = mutable.Map()

    val clientNameMap: immutable.Map[Int, Int] = Source.fromFile(new File(clientNameFile)).getLines().drop(1).map { l =>

      val lArray = l.split(",")

      val clientId = lArray(0).toInt
      val clientNameId = idMap.getOrElseUpdate(lArray(1), i.getAndIncrement)

      clientId -> clientNameId
    }.toMap

    clientNameMap
  }
}