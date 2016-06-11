package bimbo.model.clientproductgp2

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

case class ClientProductGPModel2(trainItemDAO: ItemDAO)
    extends DemandModel with LazyLogging {

  def predict(items: Seq[Item]): DenseVector[Double] = {

    val itemsByProduct = items.groupBy { i => i.productId }

    val i = new AtomicInteger(1)
    val predictedDemandByItem: Map[Item, Double] = itemsByProduct.toList.flatMap {
      case (productId, productItems) =>
        logger.info("Predicting product %d/%d".format(i.getAndIncrement, itemsByProduct.size))
        predictProductDemand(productId, productItems)
    }.toMap

    val predictedDemand = DenseVector(items.map(i => predictedDemandByItem(i)).toArray)

    predictedDemand
  }

  private def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val productMeanLogDemand = if (trainProductItems.size == 0) log(7d + 1) else {
      trainProductItems.map(i => log(i.demand + 1)).sum / trainProductItems.size
    }

    val gpModelsByClientProduct = trainProductItems.groupBy { i => getKey(i) }.map {
      case ((clientId, productId), items) =>
        val gpModel = createGprModel(items, productMeanLogDemand)
        (clientId, productId) -> gpModel
    }

    val predictedProductDemand = productItems.map { i =>

      val gpModel = gpModelsByClientProduct.get(getKey(i))
      val logDemand = gpModel match {
        case Some(gpModel) => {
          val x = DenseMatrix(1d)
          val logDemand = dk.gp.gpr.predict(x, gpModel)(0, 0)
          logDemand
        }
        case None => productMeanLogDemand
      }

      val demand = exp(logDemand) - 1

      (i, demand)
    }

    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], demandMean: Double): GprModel = {
    val x = DenseMatrix(items.map(i => 1d).toArray).t
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)
    GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, mean = demandMean)
  }

  private def getKey(item: Item): (Int, Int) = (item.clientId, item.productId)
}

