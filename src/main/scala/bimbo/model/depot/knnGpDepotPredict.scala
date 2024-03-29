package bimbo.model.depot

import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.Item
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.gpr.GprPredictEngine
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import bimbo.model.knngp2.knn.CoverTreeKnn
import dk.gp.gpr.GprModel
import breeze.linalg.DenseVector
import bimbo.model.knngp2.util.calcNewProductMap
import java.util.concurrent.atomic.AtomicInteger
import bimbo.model.knngp2.util.FeatureVectorFactory
import breeze.numerics._
import breeze.stats._
import breeze.linalg.DenseMatrix
import breeze.linalg._
import bimbo.model.knngp2.knn.LinearKnn
import bimbo.data.dao.townstate.TownState
import dk.gp.cov.CovFunc
import bimbo.model.knngp2.knn.KnnPoint
import bimbo.model.knnproductlink.trainDepotModel

object knnGpDepotPredict extends LazyLogging {

  def apply(knnModel: CoverTreeDepot, trainSize: Int, testProductItems: Seq[Item], avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO,
            townStateMap: Map[Int, TownState], clientNameMap: Map[Int, Int],
            featureVectorFactory: FeatureVectorDepotFactory, priorDemandModel: PriorLogDemandModel,
            meanLogDemand: Double,
            covFunc:CovFunc,covFuncParams:DenseVector[Double],noiseLogStdDev:Double): Seq[(Item, Double)] = {

    val predictedProductDemand = if (trainSize == 0) {
      val predictedProductDemand = testProductItems.par.map { testItem =>
        (testItem, exp(priorDemandModel.predictLogDemand(testItem)) - 1)
      }.toList
      predictedProductDemand
    } else {

      var i = new AtomicInteger(1)
      val testSize = testProductItems.size
      val predictedProductDemand = testProductItems.par.map { testItem =>

        if (i.getAndIncrement % 1000 == 0) logger.info("Predicting %d/%d".format(i.get, testSize))
        val x = featureVectorFactory.create(testItem).toDenseMatrix

        
        val trainData = List(testItem)
         val (trainedCovFuncParams,trainedLikNoiseLogStdDev) =  (covFuncParams,noiseLogStdDev)
     //     val (trainedCovFuncParams,trainedLikNoiseLogStdDev) =  trainDepotModel(trainData,knnModel,covFunc,covFuncParams,noiseLogStdDev)
        
        val trainKNNSet = knnModel.getKNN(testItem, 100)
        
        val xKnn = DenseVector.horzcat(trainKNNSet.map(_.x): _*).t
        val yKnn = DenseVector(trainKNNSet.map(point => log(point.demand + 1)).toArray)

        val model = new GprModel(xKnn, yKnn, covFunc, trainedCovFuncParams, trainedLikNoiseLogStdDev, meanFunc(_, meanLogDemand))
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