package bimbo

import breeze.linalg._
import java.io.File
import bimbo.data.CSVBimboItemDS
import breeze.numerics._
import dk.bayes.math.accuracy.rmse
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.KryoBimboItemDS

object ScoreEvalApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    logger.info("Compute rmse...")

    val predictionData = csvread(new File("target/submission.csv"), skipLines = 1)
  //  val testItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train_9.kryo").getAllItems()
     //val testItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train9_depot1911.kryo").getAllItems()
       val testItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train_9.kryo").getAllItems()//.filter(i => i.productId==42128)

    val actual = DenseVector(testItems.map(i => log(i.demand + 1)).toArray)
    val predicted = predictionData(*, ::).map(r => log(r(1) + 1))
    val rmseValue = rmse(actual, predicted)

    logger.info("rmse=%.5f".format(rmseValue))
  }
}