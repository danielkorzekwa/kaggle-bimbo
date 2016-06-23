package bimbo.model.depotproduct

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.dao.ItemByProductDAO
import dk.gp.gpr.GprModel
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.numerics._
import breeze.stats._
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import breeze.linalg.DenseMatrix
import breeze.linalg._

case class DepotProductModel(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel {
def getKey(item:Item) = item.depotId
  
  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val priorDemandModel = PriorLogDemandModel(trainProductItems, avgLogWeeklySaleDAO, null)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val gpModelsByDepot = trainProductItems.groupBy { i => getKey(i) }.map {
      case (depotId, depotItems) =>
        val gpModel = createGprModel(depotItems, meanLogDemand, priorDemandModel)
        (depotId) -> gpModel
    }
    val predictedProductDemand = testProductItems.map { item =>

      val gpModel = gpModelsByDepot.get(getKey(item))
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

    val covFunc = DepotProductCovFunc()
    val covFuncParams = DenseVector(log(1), log(1), log(1))
    val noiseLogStdDev = log(1)

    val meanVec = DenseVector.zeros[Double](x.rows) + meanLogDemand

    def meanFunc(x: DenseMatrix[Double]): DenseVector[Double] = {
      val meanVec = x(*, ::).map { x =>
        val clientId = x(1).toInt
        priorDemandModel.predictLogDemand(clientId)
     //   meanLogDemand
      }
      meanVec
    }
    new GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, meanFunc)
  }
}