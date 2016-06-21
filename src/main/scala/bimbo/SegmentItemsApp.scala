package bimbo

import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.util.saveObject
import bimbo.data.dao.AvgLogWeeklySaleDAO

object SegmentItemsApp extends LazyLogging {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/segments/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
  val itemByProductDAO = ItemByProductDAO(allItemsDAO)

  val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  def main(args: Array[String]): Unit = {

    val segmentByProductClient: mutable.Map[(Int, Int), Int] = mutable.Map()

    val productIds = List(40930) //itemByProductDAO.getProductIds()

    var segmentId = 0
    var segmentSize = 0
    productIds.foreach { productId =>
      segmentId += 1
      segmentSize = 0

      val productItems = itemByProductDAO.getProductItems(productId)

      val productItemsByClient = productItems.groupBy { item => item.clientId }

      val clientIds = productItemsByClient.keys.toList.map { clientId =>
        val clientLogSale = avgLogWeeklySaleByClientDAO.getAvgLogWeeklySaleForClient(clientId)
        clientId -> clientLogSale

      }.sortBy(_._2).map(_._1)

      clientIds.foreach { clientId =>
        val clientItems = productItemsByClient(clientId)
        val clientItemsSize = clientItems.size
        if (segmentSize + clientItemsSize > 500) {
          segmentId += 1
          segmentSize = 0
          logger.info("Number of segments=" + segmentId)
        }

        segmentSize += clientItemsSize
        segmentByProductClient += (productId, clientId) -> segmentId

      }
    }

    saveObject(segmentByProductClient.toMap, "target/segmentByProductClient.kryo")
  }
}