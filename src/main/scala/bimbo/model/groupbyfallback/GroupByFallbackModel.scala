package bimbo.model.groupbyfallback

import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import org.apache.spark.util.StatCounter

import bimbo.data.Item
import bimbo.model.DemandModel
import breeze.linalg.DenseVector
import breeze.numerics._
case class GroupByFallbackModel[T1, T2](avgLogDemandBy2:Map[T1,Double],avgLogDemandBy: Map[T1, Double], avgLogDemandFallbackBy: Map[T2, Double])(getKey: Item => T1, getFallbackKey: Item => T2) extends DemandModel {

  def predict(items: Seq[Item]): DenseVector[Double] = {
    val i = new AtomicInteger()
    def getDefaultDemand() = {
      println(i.getAndIncrement)
      log(7d + 1)
    }

    val demandSeq = items.map { i =>

      val logDemand = avgLogDemandBy.getOrElse(getKey(i), avgLogDemandFallbackBy.getOrElse(getFallbackKey(i), getDefaultDemand))
      val demand = exp(logDemand) - 1
      demand
    }.toArray
    DenseVector(demandSeq)
  }

}

object GroupByFallbackModel {

  def apply[T1, T2](trainItems: Seq[Item])(getKey: Item => T1, getFallbackKey: Item => T2): GroupByFallbackModel[T1, T2] = {

    val avgLogDemandBy2 = mutable.Map[T1, StatCounter]()
  trainItems.foreach{item =>
      avgLogDemandBy2.getOrElseUpdate(getKey(item),new StatCounter()).merge(log(item.demand + 1))
    }
    
    val avgLogDemandBy = trainItems.groupBy { i => getKey(i) }.mapValues {
      items =>
        val avgLogDemand = items.map(i => log(i.demand + 1)).sum.toDouble / items.size
        avgLogDemand
    }

    val avgLogDemandFallbackBy = trainItems.groupBy { i => getFallbackKey(i) }.mapValues {
       items =>
        val avgLogDemand = items.map(i => log(i.demand + 1)).sum.toDouble / items.size
       avgLogDemand
    }
    GroupByFallbackModel[T1, T2](avgLogDemandBy2,avgLogDemandBy, avgLogDemandFallbackBy)(getKey, getFallbackKey)
  }
}