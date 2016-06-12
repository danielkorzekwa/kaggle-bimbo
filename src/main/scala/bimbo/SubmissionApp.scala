package bimbo

import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.KryoBimboItemDS
import breeze.linalg._
import dk.gp.util.csvwrite
import bimbo.data.ItemDAO
import bimbo.data.CSVBimboItemDS
import bimbo.model.groupbyfallback.GroupByFallbackModel
import bimbo.model.clientproductgp.ClientProductGPModel

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val now = System.currentTimeMillis()
    logger.info("Generating submission...")

    val predictedDemand = predictDemand()

    logger.info("Saving submission...")
    val idColumn = DenseVector.rangeD(0, predictedDemand.size, 1)
    val predictionMat = DenseVector.horzcat(idColumn, predictedDemand)
    csvwrite("target/submission.csv", predictionMat, header = "id,Demanda_uni_equil")

    logger.info("Generating submission...done:" + (System.currentTimeMillis() - now))
  }

  def predictDemand(): DenseVector[Double] = {

    val trainItemsDS = CSVBimboItemDS("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv")
    val itemDAO = ItemDAO(trainItemsDS)

    logger.info("Loading test set...")
    val testItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train_9.kryo").getAllItems()//.filter(i => i.productId == 43174)

    logger.info("Building model...")
  //  val model = GroupByFallbackModel( itemDAO)
    val model = ClientProductGPModel(itemDAO)

    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems)

    predictedDemand
  }

}