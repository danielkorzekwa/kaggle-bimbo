package bimbo.model.clientproductgp

import breeze.linalg.DenseVector
import bimbo.data.Item
import breeze.linalg.DenseMatrix
import dk.gp.mtgpr.MtGprModel
import breeze.numerics._
import dk.gp.cov.CovSEiso
import dk.gp.mtgpr.mtgprTrain
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.model.clientproductgp.priordemand.calcProductMeanLogDemand

object trainClientProductGPModel extends LazyLogging {

  /**
   * @return (covFuncParams,noiseLogStdDev)
   */
  def apply(items: Seq[Item]): (DenseVector[Double], Double) = {

    val productMeanLogDemand = calcProductMeanLogDemand(items)

    val mtgprData = items.groupBy { i => getKey(i) }.filter(_._2.size < 500).map {
      case ((clientId, productId), items) =>

        val x = extractFeatureVec(items)
        val y = DenseVector(items.map(i => log(i.demand + 1)).toArray) - productMeanLogDemand

        DenseMatrix.horzcat(x, y.toDenseMatrix.t)
    }.toList
    logger.info("Training data:" + mtgprData.size)

    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)
    val mtGrpModel = MtGprModel(mtgprData, covFunc, covFuncParams, noiseLogStdDev)
    val trainedModel = mtgprTrain(mtGrpModel)

    (trainedModel.covFuncParams, trainedModel.likNoiseLogStdDev)
  }

  private def getKey(item: Item): (Int, Int) = (item.clientId, item.productId)
}