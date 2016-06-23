package bimbo

import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.util.saveObject
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.Item

object SegmentItemsApp extends LazyLogging {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/segments/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_8.csv", clientNamesDAO)
  val itemByProductDAO = ItemByProductDAO(allItemsDAO)

  val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_8.csv")

  def main(args: Array[String]): Unit = {

    val segmentByProductClient: mutable.Map[(Int, Int), Int] = mutable.Map()

    val productIds = List(43175)
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
        if (segmentSize + clientItemsSize > 50000) {
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
  
   def getTestItems(trainItemDAO: ItemByProductDAO, testItemDAO: ItemByProductDAO): Seq[Item] = {
    logger.info("Getting product ids for training...")
    val productIds = trainItemDAO.getProductIds().filter { productId =>
      val productSize = trainItemDAO.getProductItems(productId).size
      productSize < 500 && productSize > 0
    }

    val items = productIds.flatMap(productId => testItemDAO.getProductItems(productId))
    items
  }
}