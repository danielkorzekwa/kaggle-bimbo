package bimbo

import breeze.linalg._
import java.io.File
import breeze.numerics._
import dk.bayes.math.accuracy.rmse
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.dao.ItemDAO
import bimbo.data.dao.allitems.AllTestItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.Item

object ScoreEvalApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    logger.info("Compute rmse...")

    val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
    val trainItemDAO = ItemDAO(allItemsDAO)

    val allTestItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_9.csv", clientNamesDAO)
    val testItemByProductDAO = ItemDAO(allTestItemsDAO)

    val testItems = getTestItems(trainItemDAO,testItemByProductDAO)
    //val testItems = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_9.csv", clientNamesDAO).getAllItems() //.filter(i => i.productId == 1240)

    val predictionData = csvread(new File("target/submission.csv"), skipLines = 1)

    val actual = DenseVector(testItems.map(i => log(i.demand + 1)).toArray)
    val predicted = predictionData(*, ::).map(r => log(r(1) + 1))
    val rmseValue = rmse(actual, predicted)

    logger.info("rmse=%.5f".format(rmseValue))
  }

  def getTestItems(trainItemDAO: ItemDAO,testItemDAO:ItemDAO): Seq[Item] = {
    logger.info("Getting product ids for training...")
    val productIds = trainItemDAO.getProductIds().filter { productId =>
      val productSize = trainItemDAO.getProductItems(productId).size
      productSize < 500 && productSize > 0
    }
    
    val items = productIds.flatMap(productId => testItemDAO.getProductItems(productId))
    items
  }
}