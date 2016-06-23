package bimbo.model.segmentproduct

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.dao.ItemByProductDAO
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import breeze.linalg.DenseVector
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.linalg.DenseMatrix
import breeze.numerics._
import breeze.linalg._
import breeze.stats._
import dk.gp.gpr.GprModel

case class SegmentProductModel(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel {

  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val priorDemandModel = PriorLogDemandModel(trainProductItems, avgLogWeeklySaleDAO, null)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val segmentsByItem = SegmentsByItem(trainProductItems)

    val gpModelsBySegment = trainProductItems.groupBy { item => segmentsByItem.getSegment(item) }.map {
      case (segmentId, depotItems) =>
        val gpModel = createGprModel(depotItems, meanLogDemand, priorDemandModel)
        (segmentId) -> gpModel
    }

    val predictedProductDemand = testProductItems.map { item =>

      val gpModel = gpModelsBySegment.get(segmentsByItem.getSegment(item))
      val logDemand = gpModel match {
        case Some(gpModel) => {

          val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(5.54149)
          val x = extractFeatureVec(item, clientLogSale).toDenseMatrix
          val logDemand = dk.gp.gpr.predict(x, gpModel)(0, 0)

          logDemand
        }
        case _ => meanLogDemand
      }

      val demand = exp(logDemand) - 1

      (item, demand)
    }

    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], meanLogDemand: Double, priorDemandModel: PriorLogDemandModel): GprModel = {
    val x = extractFeatureVec(items, avgLogWeeklySaleDAO)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val covFunc = SegmentProductCovFunc()
    val covFuncParams = DenseVector(log(1), log(1), log(1),log(1))
    val noiseLogStdDev = log(1)

    val meanVec = DenseVector.zeros[Double](x.rows) + meanLogDemand

    def meanFunc(x: DenseMatrix[Double]): DenseVector[Double] = {
      val meanVec = x(*, ::).map { x =>
        val clientId = x(1).toInt
      //  priorDemandModel.predictLogDemand(clientId)
           meanLogDemand
      }
      meanVec
    }
    new GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, meanFunc)
  }
}