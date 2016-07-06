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
import bimbo.data.Item
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
import bimbo.model.knnproductlink.KnnProductLinkModel
import bimbo.data.dao.townstate.TownStateDAO

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

  def predictDemand(): DenseVector[Double] = {

    val clientNamesDAO = ClientNamesDAO("/mnt/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
    val trainItemDAO = ItemByProductDAO(allItemsDAO)

    val productMap = ProductDAO("/mnt/bimbo/producto_tabla.csv").getProductMap()

    val trainItemByPgProductDAO = ItemByPgProductDAO(allItemsDAO, productMap)

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

    val townStateMap = TownStateDAO("/mnt/bimbo/town_state.csv").getTownStateMap()
    
    logger.info("Loading test set...")
    val allTestItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_9.csv", clientNamesDAO)
    val testItemByProductDAO = ItemByProductDAO(allTestItemsDAO)
     val testItems = testItemByProductDAO.getProductItems(37360) //.filter(i => i.depotId==1387) // //
   // val testItems = allTestItemsDAO.getAllItems()
    //val testItems = getTestItems(trainItemDAO, testItemByProductDAO)

    logger.info("Building model...")
    //    val model = ClientProductGPModel(trainItemDAO, avgLogWeeklySaleByClientDAO, null)
    // val model = ClientLinkedModel(productMap, trainItemByPgProductDAO, avgLogWeeklySaleByClientDAO, trainItemDAO)
    //      val model = ClientProductHgprModel(trainItemDAO,avgLogWeeklySaleByClientDAO)
    // val model = SegmentProductModel(trainItemDAO, avgLogWeeklySaleByClientDAO)
    // val model = KnnGp2Model(trainItemDAO, avgLogWeeklySaleByClientDAO)
    val model = KnnProductLinkModel(productMap, trainItemDAO, avgLogWeeklySaleByClientDAO, trainItemByPgProductDAO,townStateMap)
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

      val productMap = ProductDAO("/mnt/bimbo/producto_tabla.csv").getProductMap()
    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_9.csv")
 val townStateMap = TownStateDAO("/mnt/bimbo/town_state.csv").getTownStateMap()
    
      val trainItemByPgProductDAO = ItemByPgProductDAO(allItemsDAO, productMap)
    
    logger.info("Loading test set...")
    val testItems = AllTestItemsDAO("/mnt/bimbo/test.csv", clientNamesDAO).getAllItems()

    logger.info("Building model...")
    //   val model = ClientProductGPModel(itemDAO, avgLogWeeklySaleByClientDAO, null)
    //val model = SegmentProductModel(itemDAO, avgLogWeeklySaleByClientDAO)
    val model = KnnProductLinkModel(productMap, itemDAO, avgLogWeeklySaleByClientDAO, trainItemByPgProductDAO,townStateMap)
    logger.info("Predicting demand...")
    val predictedDemand = model.predict(testItems)

    predictedDemand
  }

}