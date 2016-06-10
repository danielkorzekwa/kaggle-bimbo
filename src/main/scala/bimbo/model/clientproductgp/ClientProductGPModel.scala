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

case class ClientProductGPModel(gpModelsByClientProduct: Map[(Int, Int), GprModel], avgLogDemandFallbackBy: Map[Int, Double]) extends DemandModel {
  def predict(items: Seq[Item]): DenseVector[Double] = {


    val demandSeq = items.map { i =>

      val gpModel = gpModelsByClientProduct.get(getKey(i))
      val logDemand = gpModel match {
        case Some(gpModel) => {
          val x = DenseMatrix(1d)
          val logDemand = dk.gp.gpr.predict(x, gpModel)(0, 0)
          logDemand
        }
        case None => avgLogDemandFallbackBy.getOrElse(getFallbackKey(i), getDefaultDemand)
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

      val avgLogDemandFallbackBy = trainItems.groupBy { i => getFallbackKey(i) }.map {
      case (key, items) =>
        val avgLogDemand = items.map(i => log(i.demand + 1)).sum.toDouble / items.size
        key -> avgLogDemand
    }
    
    val gpModelsByClientProduct = trainItems.groupBy { i => getKey(i) }.map {
      case ((clientId,productId), items) =>

        val demandMean = avgLogDemandFallbackBy.getOrElse(productId, getDefaultDemand)
        val gpModel = createGprModel(items,demandMean)

        val avgLogDemand = items.map(i => log(i.demand + 1)).sum.toDouble / items.size
        (clientId,productId) -> gpModel
    }

  

    ClientProductGPModel(gpModelsByClientProduct, avgLogDemandFallbackBy)
  }

  private def createGprModel(items: Seq[Item],demandMean:Double): GprModel = {
    val x = DenseMatrix(items.map(i => 1d).toArray).t
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)
    GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, mean = demandMean)
  }
}