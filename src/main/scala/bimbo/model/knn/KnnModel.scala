package bimbo.model.knn

import breeze.linalg.DenseVector
import bimbo.data.Item
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import bimbo.model.depot.FeatureVectorDepotFactory
import bimbo.model.depot.knnGpDepotPredict
import bimbo.model.depot.CoverTreeDepot
import bimbo.model.knngp2.util.calcNewProductMap
import bimbo.data.dao.ItemByDepotDAO
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.data.ProductDetails
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.AvgLogPriceByProductDAO
import bimbo.data.dao.townstate.TownState
import breeze.numerics._
import bimbo.model.knnproductlink.trainDepotModel
import breeze.stats._
import bimbo.model.depot.DepotARDCovFunc
import com.typesafe.scalalogging.slf4j.LazyLogging

case class KnnModel(productMap: Map[Int, ProductDetails], trainItemDAO: ItemByProductDAO, avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO,
                    trainItemByPgProductDAO: ItemByPgProductDAO, townStateMap: Map[Int, TownState], clientNameMap: Map[Int, Int],
                    trainItemByDepotDAO: ItemByDepotDAO, avgLogPriceDAO: AvgLogPriceByProductDAO) extends LazyLogging {

  val covFunc = DepotARDCovFunc()
  // val initialCovFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1), log(1), log(1), log(1))
  //  val initialNoiseLogStdDev = log(1)

  val initialCovFuncParams = DenseVector(-0.5103220501139071, 0.516903101126618, 0.6103756648502177, 0.0, 0.0, 1.4120797754191268, 0.23460970272112433, 7.65892231205126E-5, 0.004066228222483688, 0.0)
  val initialNoiseLogStdDev = -0.955496253903561

  def predict(testItems: Seq[Item]): DenseVector[Double] = {

    val depotItems = trainItemByDepotDAO.getDepotItems(1127)
    logger.info("Depot item size:" + depotItems.size)
    val productIds = depotItems.map(i => i.productId).distinct
    logger.info("Product ids size:" + productIds.size)

    val productItems = productIds.take(10).flatMap(productId => trainItemDAO.getProductItems(productId))
    //    val productItems = trainItemDAO.getProductItems(36940)

    val trainItems = (depotItems ++ productItems).distinct
    logger.info("Train item size:" + trainItems.size)

    val predictedDemandMap = predict(trainItems, testItems).toMap

    val predictedDemand = DenseVector(testItems.map(i => predictedDemandMap(i)).toArray)

    predictedDemand
  }

  def predict(trainItems: Seq[Item], testItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainSize = trainItems.size

    val trainAndTestItems = trainItems ++ testItems
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainAndTestItems)
    val featureVectorFactory = FeatureVectorDepotFactory(avgLogWeeklySaleByClientDAO, newProductMap, townStateMap, clientNameMap, productMap, avgLogPriceDAO,null)
    val priorDemandModel = PriorLogDemandModel(trainItems, avgLogWeeklySaleByClientDAO, null)

    val knnModel = CoverTreeDepot(trainItems.toArray, initialCovFuncParams, featureVectorFactory)

    //val (trainedCovFuncParams, trainedLikNoiseLogStdDev) = trainDepotModel(knnModel, covFunc, initialCovFuncParams, initialNoiseLogStdDev)
    val (trainedCovFuncParams, trainedLikNoiseLogStdDev) = (initialCovFuncParams, initialNoiseLogStdDev) //trainDepotModel(knnModel,covFunc,initialCovFuncParams,initialNoiseLogStdDev)

    val knnModel2 = CoverTreeDepot(trainItems.toArray, trainedCovFuncParams, featureVectorFactory)

    val y = DenseVector(trainItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val predictedDemand = knnGpDepotPredict(knnModel2, trainSize, testItems, avgLogWeeklySaleByClientDAO, townStateMap, clientNameMap,
      featureVectorFactory, priorDemandModel, meanLogDemand,
      covFunc, trainedCovFuncParams, trainedLikNoiseLogStdDev)

    predictedDemand
  }
}