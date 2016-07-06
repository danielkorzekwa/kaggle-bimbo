package bimbo.model.knngp2

import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.Item
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.gpr.GprPredictEngine
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import bimbo.model.knngp2.knn.CoverTreeKnn
import dk.gp.gpr.GprModel
import breeze.linalg.DenseVector
import bimbo.model.segmentproduct.util.calcNewProductMap
import java.util.concurrent.atomic.AtomicInteger
import bimbo.model.knngp2.util.FeatureVectorFactory
import breeze.numerics._
import breeze.stats._
import breeze.linalg.DenseMatrix
import breeze.linalg._
import bimbo.model.knngp2.knn.LinearKnn
import bimbo.data.dao.townstate.TownState

object knnGpPredict extends LazyLogging {

  val covFuncParams = DenseVector(log(2), log(1.6))
  val noiseLogStdDev = log(1)

  //train 43231
  // val covFuncParams = DenseVector(-0.3340343076151246, 0.5913907578507006)
  //val noiseLogStdDev = -1.019753

  //train 37360 learn twice
  //  val covFuncParams = DenseVector(-0.9298206557839559, 0.1328678699035759)
  //val noiseLogStdDev = -0.961749
  
  //train 37360
    //val covFuncParams = DenseVector(-0.8712092877024353, 0.17592488028072195)
 // val noiseLogStdDev = -0.964787

  // val covFuncParams = DenseVector(log(1), log(1))
  // val noiseLogStdDev = log(1)
  val covFunc = KnnGP2CovFunc2()

  def apply(trainProductItems: Array[Item], testProductItems: Seq[Item], avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO,
            townStateMap: Map[Int, TownState]): Seq[(Item, Double)] = {

    logger.info("Train/test size= %d/%d".format(trainProductItems.size, testProductItems.size))

    val trainAndTestItems = trainProductItems ++ testProductItems
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainAndTestItems)
    val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap, townStateMap)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val priorDemandModel = PriorLogDemandModel(trainProductItems, avgLogWeeklySaleDAO, null)

    val trainSize = trainProductItems.size
    val predictedProductDemand = if (trainSize == 0) {
      val predictedProductDemand = testProductItems.par.map { testItem =>
        (testItem, exp(priorDemandModel.predictLogDemand(testItem)) - 1)
      }.toList
      predictedProductDemand
    } else {
      val knnModel = CoverTreeKnn(trainProductItems, covFunc, covFuncParams, featureVectorFactory)
      //   val knnModel = LinearKnn(trainProductItems, covFunc, covFuncParams, featureVectorFactory)

      var i = new AtomicInteger(1)
      val testSize = testProductItems.size
      val predictedProductDemand = testProductItems.par.map { testItem =>

        if (i.getAndIncrement % 1000 == 0) logger.info("Predicting %d/%d".format(i.get, testSize))
        val x = featureVectorFactory.create(testItem).toDenseMatrix

        val trainKNNSet = knnModel.getKNN(testItem, 100)
        //  println(trainKNNSet.size)
        val xKnn = DenseVector.horzcat(trainKNNSet.map(_.x): _*).t
        val yKnn = DenseVector(trainKNNSet.map(point => log(point.demand + 1)).toArray)

        val model = new GprModel(xKnn, yKnn, covFunc, covFuncParams, noiseLogStdDev, meanFunc(_, meanLogDemand))
        val modelEngine = GprPredictEngine(model)
        val logDemand = modelEngine.predictMean(x)(0)

        val demand = exp(logDemand) - 1
        (testItem, demand)
      }.toList
      predictedProductDemand
    }

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