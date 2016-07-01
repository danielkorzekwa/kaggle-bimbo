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

case class KnnGp2Model(trainItemDAO: ItemByProductDAO,avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel{
  
     val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1))
      val noiseLogStdDev = log(1)
   val covFunc = KnnGP2CovFunc()
   def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {
     
       logger.info("Train size=" + testProductItems.size)
       
      val trainProductItems = trainItemDAO.getProductItems(productId).toArray
      
       val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

      
       val trainAndTestItems = trainProductItems ++ testProductItems
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainAndTestItems)
    val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap)
    
    val xTrain = featureVectorFactory.create(trainProductItems)
    
     val predictedProductDemand = testProductItems.par.map { testItem =>
       val x = featureVectorFactory.create(testItem).toDenseMatrix
       
       val xTrainSortedIndexes = covFunc.cov(x,xTrain,covFuncParams).toArray.toList.zipWithIndex.sortWith((a,b) => a._1>b._1).map(_._2).take(100)
       
      val trainKNNSet = xTrainSortedIndexes.map(index => trainProductItems(index))
      
       val xKnn = featureVectorFactory.create(trainKNNSet)
    val yKnn = DenseVector(trainKNNSet.map(i => log(i.demand + 1)).toArray)

          val model = new GprModel(xKnn, yKnn, covFunc, covFuncParams, noiseLogStdDev, meanFunc(_,meanLogDemand))
        val modelEngine = GprPredictEngine(model)
     val logDemand = modelEngine.predictMean(x)(0)
      
         val demand = exp(logDemand) - 1

        (testItem, demand)
     }.toList
     predictedProductDemand
   }
     
     
    def meanFunc(x: DenseMatrix[Double],meanLogDemand:Double): DenseVector[Double] = {
      val meanVec = x(*, ::).map { x =>
        val clientId = x(1).toInt
        //  priorDemandModel.predictLogDemand(clientId)
        meanLogDemand
      }
      meanVec
    }
}