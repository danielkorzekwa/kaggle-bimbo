package bimbo.linkedmodel.client

import breeze.linalg.DenseVector
import bimbo.data.Item
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import bimbo.data.dao.AvgLogWeeklySaleDAO
import dk.gp.gpr.GprModel
import bimbo.model.clientproductgp.extractFeatureVec
import dk.gp.cov.CovSEiso
import breeze.numerics._
import dk.gp.gpr.gprPredict

case class ClientModel2(avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO) {

  def predictDemand(trainItems: Seq[Item], testItems: Seq[Item]): Seq[(Item, Double)] = {

    val priorDemandModel = PriorLogDemandModel(trainItems, avgLogWeeklySaleByClientDAO,null)

    val gpModelsByClient = trainItems.groupBy { i => i.clientId }.map {
      case (clientId, clientItems) =>
        val priorLogDemand = priorDemandModel.predictLogDemand(clientItems.head)
        val gpModel = createGprModel(clientItems, priorLogDemand)
        clientId -> gpModel
    }

    val predictedDemand = testItems.map { item =>

      val gpModel = gpModelsByClient.get(item.clientId)
      val logDemand = gpModel match {
        case Some(gpModel) => {
          val x = extractFeatureVec(item).toDenseMatrix
          val logDemand = gprPredict(x, gpModel)(0, 0)
          logDemand
        }
        case _ => priorDemandModel.predictLogDemand(item)
      }

      val demand = exp(logDemand) - 1

      (item, demand)
    }

    predictedDemand

  }

  private def createGprModel(items: Seq[Item], demandMean: Double): GprModel = {
    val x = extractFeatureVec(items)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)

    GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, mean = demandMean)
  }
}