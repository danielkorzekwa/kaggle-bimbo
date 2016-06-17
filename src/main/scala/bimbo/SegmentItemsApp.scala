package bimbo

import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.util.saveObject

object SegmentItemsApp extends LazyLogging {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/segments/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
  val itemByProductDAO = ItemByProductDAO(allItemsDAO)

  def main(args: Array[String]): Unit = {

    val productClientBySegment: mutable.Map[(Int, Int), Int] = mutable.Map()

    val productIds = itemByProductDAO.getProductIds()

    var segmentId = 0
    var segmentSize = 0
    productIds.foreach { productId =>
      segmentId += 1
      segmentSize = 0

      val productItems = itemByProductDAO.getProductItems(productId)

      val productItemsByClient = productItems.groupBy { item => item.clientId }.toList.sortBy(_._2.size)

      productItemsByClient.foreach {
        case (clientId, clientItems) =>

          val clientItemsSize = clientItems.size
          if (segmentSize + clientItemsSize > 500) {
            segmentId += 1
            segmentSize = 0
            logger.info("Number of segments=" + segmentId)
          }

          segmentSize += clientItemsSize
          productClientBySegment += (productId, clientId) -> segmentId

      }
    }

    saveObject(productClientBySegment.toMap, "target/productClientBySegment.kryo")
  }
}