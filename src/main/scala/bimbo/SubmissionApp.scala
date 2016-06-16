package bimbo

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg._
import dk.gp.util.csvwrite
import bimbo.data.dao.ItemDAO
import bimbo.model.groupbyfallback.GroupByFallbackModel
import bimbo.model.clientproductgp.ClientProductGPModel
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.allitems.AllTestItemsDAO
import bimbo.data.dao.allitems.AllTestItemsDAO
import bimbo.data.dao.ItemDAO
import bimbo.model.productgp.ProductGPModel

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val now = System.currentTimeMillis()
    logger.info("Generating submission...")

    val predictedDemand = predictDemand()

    logger.info("Saving submission...")
    val idColumn = if(predictedDemand.size==1) DenseVector(0.0) else DenseVector.rangeD(0, predictedDemand.size, 1)
    val predictionMat = DenseVector.horzcat(idColumn, predictedDemand)
    csvwrite("target/submission.csv", predictionMat, header = "id,Demanda_uni_equil")

    logger.info("Generating submission...done:" + (System.currentTimeMillis() - now))
  }

  def predictDemand(): DenseVector[Double] = {

    val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
    val itemDAO = ItemDAO(allItemsDAO)

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

    logger.info("Loading test set...")
    val allTestItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_9.csv", clientNamesDAO)
     val testItems = ItemDAO(allTestItemsDAO).getProductItems( 32295)
    //val testItems = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_9.csv", clientNamesDAO).getAllItems()//.filter(i => i.productId == 1240)

    logger.info("Building model...")
    //  val model = GroupByFallbackModel( itemDAO)
    val model = ProductGPModel(itemDAO, avgLogWeeklySaleByClientDAO,"target/productGPModelParams.kryo")

    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems)//.map(d => "%.0f".format(d).toDouble)

    predictedDemand
  }

  def predictDemandSubmission(): DenseVector[Double] = {

    val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/segments/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/train.csv", clientNamesDAO)
    val itemDAO = ItemDAO(allItemsDAO)

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_9.csv")

    logger.info("Loading test set...")
    val testItems = AllTestItemsDAO("c:/perforce/daniel/bimbo/test.csv", clientNamesDAO).getAllItems()

    logger.info("Building model...")
    val model = ClientProductGPModel(itemDAO, avgLogWeeklySaleByClientDAO)

    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems)

    predictedDemand
  }

}