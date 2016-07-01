package bimbo.model.knngp2

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.dao.ItemByProductDAO
import bimbo.model.segmentproduct.util.calcNewProductMap
import bimbo.model.knngp2.util.FeatureVectorFactory
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.numerics._
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.stats._
import dk.gp.gpr.GprModel
import dk.gp.gpr.GprPredictEngine
import bimbo.model.knngp2.util.ItemClusterBuilder
import bimbo.model.knngp2.knn.BruteKnn
import bimbo.model.knngp2.knn.ClusterKnn
import java.util.concurrent.atomic.AtomicInteger

case class KnnGp2Model(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel {

  val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1))
  val noiseLogStdDev = log(1)
  val covFunc = KnnGP2CovFunc()
  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId).toArray
    logger.info("Train/test size= %d/%d".format(trainProductItems.size, testProductItems.size))

    val trainAndTestItems = trainProductItems ++ testProductItems
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainAndTestItems)
    val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap)

//    val itemClusterBuilder = ItemClusterBuilder(covFunc, covFuncParams, threshold = 0.2 * 6, featureVectorFactory)
//
//    trainProductItems.foreach { item =>
//      itemClusterBuilder.processItem(item)
//    }
//    println("num of clusters=" + itemClusterBuilder.getClusters().size)
//    itemClusterBuilder.getClusters().toList.sortBy(_._2.size).foreach { case (item, items) => println(items.size) }

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val knnModel = BruteKnn(trainProductItems, covFunc, covFuncParams, featureVectorFactory)
   // val knnModel = ClusterKnn(trainProductItems, covFunc, covFuncParams, featureVectorFactory)
    
    var i = new AtomicInteger(1)
    val testSize = testProductItems.size
    val predictedProductDemand = testProductItems.par.map { testItem =>
      
      if(i.getAndIncrement % 10 ==0) logger.info("Predicting %d/%d".format(i.get,testSize))
      val x = featureVectorFactory.create(testItem).toDenseMatrix

      val trainKNNSet = knnModel.getKNN(testItem, 100)

      val xKnn = featureVectorFactory.create(trainKNNSet)
      val yKnn = DenseVector(trainKNNSet.map(i => log(i.demand + 1)).toArray)

      val model = new GprModel(xKnn, yKnn, covFunc, covFuncParams, noiseLogStdDev, meanFunc(_, meanLogDemand))
      val modelEngine = GprPredictEngine(model)
      val logDemand = modelEngine.predictMean(x)(0)

      val demand = exp(logDemand) - 1

      (testItem, demand)
    }.toList
    predictedProductDemand
  }

  def meanFunc(x: DenseMatrix[Double], meanLogDemand: Double): DenseVector[Double] = {
    val meanVec = x(*, ::).map { x =>
      val clientId = x(1).toInt
      //  priorDemandModel.predictLogDemand(clientId)
      meanLogDemand
    }
    meanVec
  }
}