package bimbo.model.depot

import breeze.linalg.DenseVector
import bimbo.data.Item
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.PgProductDetails
import bimbo.data.GenericProductDetails
import bimbo.data.ProductDetails
import bimbo.data.dao.townstate.TownState
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.dao.ItemByDepotDAO
import breeze.numerics._
import bimbo.model.knngp2.knnGpPredict
import bimbo.model.knngp2.knn.CoverTreeKnn
import bimbo.model.knngp2.util.calcNewProductMap
import bimbo.model.knngp2.knnGpPredict
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import breeze.stats._

case class DepotModel(productMap: Map[Int, ProductDetails], trainItemDAO: ItemByProductDAO, avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO,
                      trainItemByPgProductDAO: ItemByPgProductDAO, townStateMap: Map[Int, TownState], clientNameMap: Map[Int, Int],
                      trainItemByDepotDAO: ItemByDepotDAO) extends LazyLogging {

  val covFuncParams = DenseVector(log(2), log(1.6))
  val noiseLogStdDev = log(1)

  val covFunc = DepotCovFunc()

  def predict(testItems: Seq[Item]): DenseVector[Double] = {
    val itemsByDepot = testItems.groupBy { i => i.depotId }

    val i = new AtomicInteger(1)
    val predictedDemandByItem: Map[Item, Double] = itemsByDepot.toList.flatMap {
      case (depotId, depotTestItems) =>
        if (i.getAndIncrement % 10 == 0) logger.info("Predicting depot %d/%d".format(i.get, itemsByDepot.size))
        predictDepotDemand(depotId, depotTestItems)
    }.toMap

    val predictedDemand = DenseVector(testItems.map(i => predictedDemandByItem(i)).toArray)

    predictedDemand
  }

  def predictDepotDemand(depotId: Int, depotTestItems: Seq[Item]): Seq[(Item, Double)] = {

    val depotTrainItems = trainItemByDepotDAO.getDepotItems(depotId)

    val predictedDemand = predictPG(depotTrainItems, depotTestItems)

    predictedDemand
  }

  def predictPG(depotTrainItems: Seq[Item], testItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainSize = depotTrainItems.size

    val trainAndTestItems = depotTrainItems ++ testItems
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainAndTestItems)
    val featureVectorFactory = FeatureVectorDepotFactory(avgLogWeeklySaleByClientDAO, newProductMap, townStateMap, clientNameMap,productMap)
    val priorDemandModel = PriorLogDemandModel(depotTrainItems, avgLogWeeklySaleByClientDAO, null)

    val knnModel = CoverTreeDepot(depotTrainItems.toArray, covFunc, covFuncParams, featureVectorFactory)

    val y = DenseVector(depotTrainItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val predictedDemand =  knnGpDepotPredict(knnModel, trainSize, testItems, avgLogWeeklySaleByClientDAO, townStateMap, clientNameMap,
          featureVectorFactory, priorDemandModel, meanLogDemand)
    
    predictedDemand
  }

  
}