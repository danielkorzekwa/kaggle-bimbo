package bimbo

import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.ds.KryoBimboItemDS
import breeze.linalg._
import dk.gp.util.csvwrite
import bimbo.data.dao.ItemDAO
import bimbo.model.groupbyfallback.GroupByFallbackModel
import bimbo.model.clientproductgp.ClientProductGPModel
import bimbo.data.ds.KryoBimboItemDS
import bimbo.data.ds.CSVBimboItemDS
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.ds.CSVBimboTestItemDS

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

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

    logger.info("Loading test set...")
     val testItems = KryoBimboItemDS("c:/perforce/daniel/bimbo/segments/train_9.kryo").getAllItems().filter(i => i.productId == 2233)

    logger.info("Building model...")
    //  val model = GroupByFallbackModel( itemDAO)
    val model = ClientProductGPModel(itemDAO, avgLogWeeklySaleByClientDAO)

    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems)

    predictedDemand
  }
  
   def predictDemandSubmission(): DenseVector[Double] = {

    val trainItemsDS = CSVBimboItemDS("c:/perforce/daniel/bimbo/train.csv")
    val itemDAO = ItemDAO(trainItemsDS)

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_9.csv")

    logger.info("Loading test set...")
    val testItems = CSVBimboTestItemDS("c:/perforce/daniel/bimbo/test.csv").getAllItems()

    logger.info("Building model...")
    val model = ClientProductGPModel(itemDAO, avgLogWeeklySaleByClientDAO)

    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems)

    predictedDemand
  }

}