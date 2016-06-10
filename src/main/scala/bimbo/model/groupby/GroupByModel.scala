package bimbo.model.groupby

import bimbo.model.DemandModel
import breeze.linalg.DenseVector
import bimbo.data.Item
import breeze.numerics._
import GroupByModel._
import java.util.concurrent.atomic.AtomicInteger

case class GroupByModel[T](avgLogDemandBy: Map[T, Double],getKey:Item => T) extends DemandModel {

  def predict(items: Seq[Item]): DenseVector[Double] = {
   val i=new AtomicInteger()
    def  getDefaultDemand() = {
      println(i.getAndIncrement)
      log(7d + 1)
    }
    val demandSeq = items.map { i =>

      val logDemand = avgLogDemandBy.getOrElse(getKey(i), getDefaultDemand)
      val demand = exp(logDemand) - 1
      demand
    }.toArray
    DenseVector(demandSeq)
  }
}

object GroupByModel {

 

  def apply[T](trainItems: Seq[Item])(getKey:Item => T): GroupByModel[T] = {

    val avgLogDemandBy = trainItems.groupBy { i => getKey(i) }.map {
      case (key, items) =>
        val avgLogDemand = items.map(i => log(i.demand + 1)).sum.toDouble / items.size
        key -> avgLogDemand
    }
    GroupByModel[T](avgLogDemandBy,getKey)
  }
}