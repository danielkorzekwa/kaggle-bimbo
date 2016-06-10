package bimbo

import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.CSVBimboTestItemDS
import bimbo.model.baseline.BaselineModel
import breeze.linalg._
import dk.gp.util.csvwrite
import bimbo.data.CSVBimboItemDS
import bimbo.model.groupby.GroupByModel
import bimbo.data.KryoBimboItemDS
import bimbo.model.groupbyfallback.GroupByFallbackModel
import bimbo.model.clientproductgp.ClientProductGPModel

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val now = System.currentTimeMillis()
    logger.info("Generating submission...")

        logger.info("Loading train set...")
        val trainItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train3_8_depot1911.kryo").getAllItems()
        logger.info("Loading test set...")
        val testItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train9_depot1911.kryo").getAllItems()

//    logger.info("Loading train set...")
//    val trainItems = CSVBimboItemDS("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv").getAllItems()
//    logger.info("Loading test set...")
//    val testItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train_9.kryo").getAllItems()

    logger.info("Building model...")
    val model = GroupByFallbackModel[(Int, Int), Int](trainItems)(i => (i.clientId, i.productId), i => (i.productId))
    //  val model = ClientProductGPModel(trainItems)
    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems)

    logger.info("Saving submission...")
    val idColumn = DenseVector.rangeD(0, testItems.size, 1)
    val predictionMat = DenseVector.horzcat(idColumn, predictedDemand)
    csvwrite("target/submission.csv", predictionMat, header = "id,Demanda_uni_equil")

    logger.info("Generating submission...done:" + (System.currentTimeMillis() - now))
  }

}