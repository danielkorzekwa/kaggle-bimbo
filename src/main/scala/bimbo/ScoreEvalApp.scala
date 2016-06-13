package bimbo

import breeze.linalg._
import java.io.File
import bimbo.data.ds.CSVBimboItemDS
import breeze.numerics._
import dk.bayes.math.accuracy.rmse
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.ds.KryoBimboItemDS
import bimbo.data.dao.ItemDAO

object ScoreEvalApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    logger.info("Compute rmse...")
 val trainItemsDS = CSVBimboItemDS("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv")
    val itemDAO = ItemDAO(trainItemsDS)

    
    val predictionData = csvread(new File("target/submission.csv"), skipLines = 1)
    val testItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train_9.kryo").getAllItems().filter(i => i.productId==2233)

    val actual = DenseVector(testItems.map(i => log(i.demand + 1)).toArray)
    val predicted = predictionData(*, ::).map(r => log(r(1) + 1))
    val rmseValue = rmse(actual, predicted)

    logger.info("rmse=%.5f".format(rmseValue))
  }
}