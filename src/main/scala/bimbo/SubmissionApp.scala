package bimbo

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg._
import dk.gp.util.csvwrite
import bimbo.model.clientproductgp.ClientProductGPModel
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.allitems.AllTestItemsDAO
import bimbo.data.dao.allitems.AllTestItemsDAO
import bimbo.model.productgp.ProductGPModel
import bimbo.data.Item
import bimbo.data.dao.ItemSegmentDAO
import bimbo.data.dao.SegmentGPParamsDAO
import bimbo.data.dao.ItemByProductDAO
import bimbo.linkedmodel.LinkedDemandModel
import bimbo.data.dao.ProductDAO
import bimbo.linkedmodel.client.ClientLinkedModel
import bimbo.linkedmodel.client.ClientLinkedModel
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.model.clientproducthgpr.ClientProductHgprModel
import bimbo.model.clientproducthgpr.ClientProductHgprModel
import bimbo.data.dao.AvgLogDemandByClientDAO
import bimbo.model.segmentproduct.SegmentProductModel
import bimbo.model.knngp2.KnnGp2Model

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val now = System.currentTimeMillis()
    logger.info("Generating submission...")

    // val predictedDemand = predictDemand().map(demand => "%.10f".format(demand.max(0)))
    val predictedDemand = predictDemand().map(demand => demand.toString)

    logger.info("Saving submission...")
    val idColumn = if (predictedDemand.size == 1) DenseVector(0.toString) else DenseVector.rangeD(0, predictedDemand.size, 1).map(x => "%.0f".format(x))
    val predictionMat = DenseVector.horzcat(idColumn, predictedDemand)
    csvwrite("target/submission.csv", predictionMat, header = "id,Demanda_uni_equil")

    logger.info("Generating submission...done:" + (System.currentTimeMillis() - now))
  }

  def predictDemandLinkedProduct(): DenseVector[Double] = {
    val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
    val productMap = ProductDAO("c:/perforce/daniel/bimbo/producto_tabla.csv").getProductMap()

    //train data
    val trainItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_8.csv", clientNamesDAO)
    val trainItemByPgProductDAO = ItemByPgProductDAO(trainItemsDAO, productMap)
    val trainItemByProductDAO = ItemByProductDAO(trainItemsDAO)
    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_8.csv")

    val model = ClientLinkedModel(productMap, trainItemByPgProductDAO, avgLogWeeklySaleByClientDAO, trainItemByProductDAO)

    //test data
    logger.info("Loading test set...")
    val testItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_9.csv", clientNamesDAO)
    val testItemByProductDAO = ItemByProductDAO(testItemsDAO)
    val testItems = testItemByProductDAO.getProductItems(43231)
    //val testItems = testItemsDAO.getAllItems()
    val predictedDemand = model.predict(testItems)

    predictedDemand
  }

  def predictDemand(): DenseVector[Double] = {

    val clientNamesDAO = ClientNamesDAO("/mnt/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
    val trainItemDAO = ItemByProductDAO(allItemsDAO)

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

    logger.info("Creating itemSegmentDAO")
    // val itemSegmentDAO = ItemSegmentDAO("target/segmentByProductClient.kryo")

    logger.info("Creating segmentGPParamsDAO")
    // val segmentGPParamsDAO = SegmentGPParamsDAO("target/segmentGPModelParams.kryo")

    logger.info("Loading test set...")
    val allTestItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_9.csv", clientNamesDAO)
    val testItemByProductDAO = ItemByProductDAO(allTestItemsDAO)
  //    val testItems = testItemByProductDAO.getProductItems(43231)//.filter(i => i.depotId==1387) // //
    val testItems = allTestItemsDAO.getAllItems()
    //val testItems = getTestItems(trainItemDAO, testItemByProductDAO)

    logger.info("Building model...")
    //    val model = GroupByFallbackModel( trainItemDAO)
    //   val model = ClientProductGPModel(trainItemDAO, avgLogWeeklySaleByClientDAO, null)
    //      val model = ClientProductHgprModel(trainItemDAO,avgLogWeeklySaleByClientDAO)
    //      val model = ProductGPModel(trainItemDAO, avgLogWeeklySaleByClientDAO, "target/productGPModelParams.kryo",itemSegmentDAO,segmentGPParamsDAO)
    // val model = SegmentProductModel(trainItemDAO, avgLogWeeklySaleByClientDAO)
    val model = KnnGp2Model(trainItemDAO, avgLogWeeklySaleByClientDAO)

    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems) //.map(d => "%.0f".format(d).toDouble)

    predictedDemand
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

  def predictDemandSubmission(): DenseVector[Double] = {

    val clientNamesDAO = ClientNamesDAO("/mnt/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("/mnt/bimbo/train.csv", clientNamesDAO)
    val itemDAO = ItemByProductDAO(allItemsDAO)

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_9.csv")

    logger.info("Loading test set...")
    val testItems = AllTestItemsDAO("/mnt/bimbo/test.csv", clientNamesDAO).getAllItems()

    logger.info("Building model...")
    //   val model = ClientProductGPModel(itemDAO, avgLogWeeklySaleByClientDAO, null)
    val model = SegmentProductModel(itemDAO, avgLogWeeklySaleByClientDAO)
    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems)

    predictedDemand
  }

}