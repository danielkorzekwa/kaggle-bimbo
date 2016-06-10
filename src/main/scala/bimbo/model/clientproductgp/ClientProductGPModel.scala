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
import ClientProductGPModel._
import java.util.concurrent.atomic.AtomicInteger
import org.apache.spark.util.StatCounter
import scala.collection._

case class ClientProductGPModel(gpModelsByClientProduct: Map[(Int, Int), GprModel], avgLogDemandFallbackBy: Map[Int, StatCounter]) extends DemandModel {
  def predict(items: Seq[Item]): DenseVector[Double] = {

    val demandSeq = items.map { i =>

      val gpModel = gpModelsByClientProduct.get(getKey(i))
      val logDemand = gpModel match {
        case Some(gpModel) => {
          val x = DenseMatrix(1d)
          val logDemand = dk.gp.gpr.predict(x, gpModel)(0, 0)
          logDemand
        }
        case None => {
          avgLogDemandFallbackBy.get(getFallbackKey(i)) match {
            case Some(fallbackStatCounter) => fallbackStatCounter.mean
            case None                      => getDefaultDemand

          }
        }
      }

      exp(logDemand) - 1
    }.toArray
    DenseVector(demandSeq)
  }
}

object ClientProductGPModel {

  val i = new AtomicInteger()
  def getDefaultDemand() = {
    println(i.getAndIncrement)
    log(7d + 1)
  }

  def getKey(item: Item): (Int, Int) = (item.clientId, item.productId)
  def getFallbackKey(item: Item): Int = item.productId

  def apply(trainItems: Seq[Item]): ClientProductGPModel = {

    val avgLogDemandFallbackBy = mutable.Map[Int, StatCounter]()
    trainItems.foreach { item =>
      avgLogDemandFallbackBy.getOrElseUpdate(getFallbackKey(item), new StatCounter()).merge(log(item.demand + 1))
    }

    val gpModelsByClientProduct = trainItems.groupBy { i => getKey(i) }.map {
      case ((clientId, productId), items) =>

        val demandMean = avgLogDemandFallbackBy.get(productId) match {
          case Some(fallbackStatCounter) => fallbackStatCounter.mean
          case None                      => getDefaultDemand

        }

        val gpModel = createGprModel(items, demandMean)

        val avgLogDemand = items.map(i => log(i.demand + 1)).sum.toDouble / items.size
        (clientId, productId) -> gpModel
    }

    ClientProductGPModel(gpModelsByClientProduct, avgLogDemandFallbackBy)
  }

  private def createGprModel(items: Seq[Item], demandMean: Double): GprModel = {
    val x = DenseMatrix(items.map(i => 1d).toArray).t
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)
    println(x.rows)
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)
    GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, mean = demandMean)
  }
}