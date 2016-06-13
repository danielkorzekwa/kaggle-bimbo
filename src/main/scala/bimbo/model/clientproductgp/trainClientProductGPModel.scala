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
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import bimbo.data.dao.AvgLogWeeklySaleDAO

object trainClientProductGPModel extends LazyLogging {

  /**
   * @return (covFuncParams,noiseLogStdDev)
   */
  def apply(productItems: Seq[Item], avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO): (DenseVector[Double], Double) = {

    logger.info("Number of items:" + productItems.size)
    
    val productItemsByClient = productItems.groupBy { i => getKey(i) }.filter{case (key,items) => items.size > 0 && items.size<1000}
    
     val priorDemandModel = PriorLogDemandModel(productItems, avgLogWeeklySaleByClientDAO)
     
    val mtgprData = productItemsByClient.map {
      case ((clientId, productId), clientProductItems) =>

        val x = extractFeatureVec(clientProductItems)

        val priorLogDemand = priorDemandModel.predictLogDemand(clientProductItems.head)
        val y = DenseVector(clientProductItems.map(i => log(i.demand + 1)).toArray) - priorLogDemand

        DenseMatrix.horzcat(x, y.toDenseMatrix.t)
    }.toList
    logger.info("Training data:" + mtgprData.size)

    val covFunc = RouteCovFunc()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)
    val mtGrpModel = MtGprModel(mtgprData, covFunc, covFuncParams, noiseLogStdDev)
    val trainedModel = mtgprTrain(mtGrpModel)

    (trainedModel.covFuncParams, trainedModel.likNoiseLogStdDev)
  }

  private def getKey(item: Item): (Int, Int) = (item.clientId, item.productId)
}