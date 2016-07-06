package bimbo

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging

import bimbo.data.Item
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import breeze.linalg.{ * => * }
import breeze.linalg.DenseVector
import breeze.linalg.csvread
import breeze.numerics.log
import dk.bayes.math.accuracy.rmse

object ScoreEvalApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    logger.info("Compute rmse...")

    val clientNamesDAO = ClientNamesDAO("/mnt/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
    val trainItemDAO = ItemByProductDAO(allItemsDAO)

    val allTestItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_9.csv", clientNamesDAO)
    val testItemByProductDAO = ItemByProductDAO(allTestItemsDAO)

   // val testItems = testItemByProductDAO.getProductItems(40931)//.filter(i => i.depotId==1387) //getTestwItems(trainItemDAO, testItemByProductDAO)//testItemByProductDAO.getProductItems(1278)// //testItemByProductDAO.getProductItems(43175) 
   //val testItems = getTestItems(trainItemDAO, testItemByProductDAO)
       val testItems = allTestItemsDAO.getAllItems()

    val predictionData = csvread(new File("target/submission.csv"), skipLines = 1)

    val actual = DenseVector(testItems.map(i => log(i.demand + 1)).toArray)
    val predicted = predictionData(*, ::).map(r => log(r(1) + 1))
    val rmseValue = rmse(actual, predicted)

    logger.info("rmse=%.5f".format(rmseValue))
  }

  def getTestItems(trainItemDAO: ItemByProductDAO, testItemDAO: ItemByProductDAO): Seq[Item] = {
    logger.info("Getting product ids for training...")
    val productIds = trainItemDAO.getProductIds().filter { productId =>
      val productSize = trainItemDAO.getProductItems(productId).size
      productSize < 1000 && productSize > 0
    }

    val items = productIds.flatMap(productId => testItemDAO.getProductItems(productId))
    items
  }
}