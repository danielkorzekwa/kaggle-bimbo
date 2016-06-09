package bimbo.model.groupby

import bimbo.model.DemandModel
import breeze.linalg.DenseVector
import bimbo.data.Item
import breeze.numerics._

case class GroupByModel(logDemandByProduct: Map[Int, Double]) extends DemandModel {

  def predict(items: Seq[Item]): DenseVector[Double] = {
    val defaultDemand = log(7d+1)
    val demandSeq = items.map{i => 
      
      val logDemand = logDemandByProduct.getOrElse(i.productId, defaultDemand)
      val demand = exp(logDemand)-1
      demand
      }.toArray
    DenseVector(demandSeq)
  }
}

object GroupByModel {

  def apply(trainItems: Seq[Item]): GroupByModel = {

    val avgLogDemandByProduct = trainItems.groupBy { i => i.productId }.map {
      case (productId, items) =>
        val avgLogDemand = items.map(i => log(i.demand + 1)).sum.toDouble / items.size
        productId -> avgLogDemand
    }
    GroupByModel(avgLogDemandByProduct)
  }
}