package bimbo.data.dao.allitems

import bimbo.data.Item
import scala.io.Source
import java.io.File
import bimbo.data.dao.ClientNamesDAO

case class AllTrainItemsDAO(itemsFile: String, clientNamesDAO: ClientNamesDAO) extends AllItemsDAO{

  def getAllItems(): Seq[Item] = {

    val clientsNameByClientIdMap = clientNamesDAO.getClientNamesMap()

    val items: Seq[Item] = Source.fromFile(new File(itemsFile)).getLines().drop(1).map { l =>
      val item: Item = createItem(l, clientsNameByClientIdMap)
      item
    }.toList

    items
  }

  private def createItem(l: String, clientsNameByClientIdMap: Map[Int, String]): Item = {
    val lArray = l.split(",")

    val weekId = lArray(0).toInt
    val depotId = lArray(1).toInt
    val channelId = lArray(2).toInt
    val routeId = lArray(3).toInt
    val clientId = lArray(4).toInt
    val productId = lArray(5).toInt
    val ret = lArray(8).toDouble
    val demand = lArray(10).toDouble

    val clientName = clientsNameByClientIdMap(clientId)

    val item = Item(weekId, depotId, channelId, routeId, clientId, clientName, productId, ret,demand)

    item
  }
}