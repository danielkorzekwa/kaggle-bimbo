package bimbo.data.dao

import scala.io.Source
import java.io.File

case class ClientNamesDAO(clientNameFile: String) {

  def getClientNamesMap(): Map[Int, String] = {
    val clientNamesMap: Map[Int, String] = Source.fromFile(new File(clientNameFile)).getLines().drop(1).map { l =>

      val lArray = l.split(",")

      val clientId = lArray(0).toInt
      val clientName = lArray(1)
      clientId -> clientName
    }.toMap

    clientNamesMap
  }
}