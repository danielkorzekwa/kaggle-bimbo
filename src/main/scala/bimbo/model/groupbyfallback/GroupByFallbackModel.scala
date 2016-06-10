package bimbo.model.groupbyfallback

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.Map
import scala.collection.Seq
import scala.collection.mutable

import org.apache.spark.util.StatCounter

import bimbo.data.Item
import bimbo.model.DemandModel
import breeze.linalg.DenseVector
import breeze.numerics.exp
import breeze.numerics.log
case class GroupByFallbackModel[T1, T2](avgLogDemandBy: Map[T1, StatCounter], avgLogDemandFallbackBy: Map[T2, StatCounter])(getKey: Item => T1, getFallbackKey: Item => T2) extends DemandModel {

  def predict(items: Seq[Item]): DenseVector[Double] = {
    val i = new AtomicInteger()
    def getDefaultDemand() = {
      println(i.getAndIncrement)
      log(7d + 1)
    }

    val demandSeq = items.map { i =>

      val logDemand = avgLogDemandBy.get(getKey(i)) match {
        case Some(statCounter) => statCounter.mean
        case None => {
          avgLogDemandFallbackBy.get(getFallbackKey(i)) match {
            case Some(fallbackStatCounter) => fallbackStatCounter.mean
            case None                      => getDefaultDemand

          }

        }
      }

      val demand = exp(logDemand) - 1
      demand
    }.toArray
    DenseVector(demandSeq)
  }

}

object GroupByFallbackModel {

  def apply[T1, T2](trainItems: Seq[Item])(getKey: Item => T1, getFallbackKey: Item => T2): GroupByFallbackModel[T1, T2] = {

    val avgLogDemandBy = mutable.Map[T1, StatCounter]()
    trainItems.foreach { item =>
      avgLogDemandBy.getOrElseUpdate(getKey(item), new StatCounter()).merge(log(item.demand + 1))
    }

    val avgLogDemandFallbackBy = mutable.Map[T2, StatCounter]()
    trainItems.foreach { item =>
      avgLogDemandFallbackBy.getOrElseUpdate(getFallbackKey(item), new StatCounter()).merge(log(item.demand + 1))
    }

    GroupByFallbackModel[T1, T2](avgLogDemandBy, avgLogDemandFallbackBy)(getKey, getFallbackKey)
  }
}