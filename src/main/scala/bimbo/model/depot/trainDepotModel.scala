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
import bimbo.model.depot.CoverTreeDepot
import bimbo.data.Item

object trainDepotModel {

  /**
   * @return (covFuncParams, loglikStdDev)
   */
  def apply(coverTree: CoverTreeDepot, covFunc: CovFunc, covFuncParams: DenseVector[Double], noiseLogStdDev: Double): (DenseVector[Double], Double) = {

    val trainItems = new Random(3450).shuffle(coverTree.trainSet.toSeq).take(100)

    apply(trainItems, coverTree, covFunc, covFuncParams, noiseLogStdDev)

  }

  /**
   * @return (covFuncParams, loglikStdDev)
   */
  def apply(trainItems: Seq[Item], coverTree: CoverTreeDepot, covFunc: CovFunc, covFuncParams: DenseVector[Double], noiseLogStdDev: Double): (DenseVector[Double], Double) = {

    val y = DenseVector(coverTree.trainSet.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val data = trainItems.map { item =>

      val trainItems = coverTree.getKNN(item, 100)
      val xKnn = DenseVector.horzcat(trainItems.map(_.x): _*).t
      val yKnn = DenseVector(trainItems.map(point => log(point.demand + 1)).toArray) - meanLogDemand
      DenseMatrix.horzcat(xKnn, yKnn.toDenseMatrix.t)
    }

    val mtgprModel = MtGprModel(data, covFunc, covFuncParams, noiseLogStdDev)

    val trainedModel = mtgprTrain(mtgprModel, maxIter = 20)

    (trainedModel.covFuncParams, trainedModel.likNoiseLogStdDev)
  }

}