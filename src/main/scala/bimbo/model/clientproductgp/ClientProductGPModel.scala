package bimbo.model.clientproductgp

import bimbo.model.DemandModel
import breeze.linalg.DenseVector
import bimbo.data.Item
import dk.gp.gpr.GprModel
import dk.gp.gpr.GprModel
import breeze.linalg.DenseMatrix
import breeze.numerics._
import dk.gp.cov.CovSEiso
import dk.gp.gpr.GprModel
import dk.gp.gpr.GprModel
import java.util.concurrent.atomic.AtomicInteger
import org.apache.spark.util.StatCounter
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.ItemDAO
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel

case class ClientProductGPModel(trainItemDAO: ItemDAO)
    extends DemandModel with LazyLogging {

  def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val priorDemandModel = PriorLogDemandModel(trainProductItems)

    val gpModelsByClientProduct = trainProductItems.groupBy { i => getKey(i) }.map {
      case ((clientId, productId), items) =>
        val priorLogDemand = priorDemandModel.predictLogDemand(items.head)
        val gpModel = createGprModel(items, priorLogDemand)
        (clientId, productId) -> gpModel
    }

    val predictedProductDemand = productItems.map { item =>

      val gpModel = gpModelsByClientProduct.get(getKey(item))
      val logDemand = gpModel match {
        case Some(gpModel)=> {
          val x = extractFeatureVec(item).toDenseMatrix
          val logDemand = dk.gp.gpr.predict(x, gpModel)(0, 0)
          logDemand
        }
        case _ => priorDemandModel.predictLogDemand(item)
      }

      val demand = exp(logDemand) - 1

      (item, demand)
    }

    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], demandMean: Double): GprModel = {
    val x = DenseVector.horzcat(items.map(i => extractFeatureVec(i)): _*).t
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)
    GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, mean = demandMean)
  }

  private def getKey(item: Item): (Int, Int) = (item.clientId, item.productId)
}

