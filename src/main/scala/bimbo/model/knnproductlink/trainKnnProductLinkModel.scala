package bimbo.model.knnproductlink

import breeze.linalg.DenseVector
import bimbo.model.knngp2.knn.CoverTreeKnn
import breeze.linalg.DenseMatrix
import dk.gp.mtgpr.mtgprTrain
import dk.gp.mtgpr.MtGprModel
import scala.util.Random
import breeze.linalg._
import breeze.stats._
import breeze.numerics._
import bimbo.model.knngp2.KnnARDCovFunc
import dk.gp.cov.CovFunc

object trainKnnProductLinkModel {
  
  /**
   * @return (covFuncParams, loglikStdDev)
   */
  def apply(coverTree:CoverTreeKnn,covFunc:CovFunc,covFuncParams:DenseVector[Double],noiseLogStdDev:Double):(DenseVector[Double],Double) = {
    
   
 
    val trainItems = coverTree.trainSet.toList
    
     val y = DenseVector(trainItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    
    val data = new Random(3450).shuffle(trainItems).take(100).map { item =>

      val trainItems = coverTree.getKNN(item, 100)
      val xKnn = DenseVector.horzcat(trainItems.map(_.x): _*).t
      val yKnn = DenseVector(trainItems.map(point => log(point.demand + 1)).toArray) - meanLogDemand
      DenseMatrix.horzcat(xKnn, yKnn.toDenseMatrix.t)
    }

    val mtgprModel = MtGprModel(data, covFunc, covFuncParams, noiseLogStdDev)

    val trainedModel = mtgprTrain(mtgprModel, maxIter = 20)
    
    (trainedModel.covFuncParams,trainedModel.likNoiseLogStdDev)
  }
}