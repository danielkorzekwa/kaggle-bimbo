package bimbo.util

import org.apache.spark.util.StatCounter
import scala.collection._

case class StatCounterByKey[T, K](items: Seq[T])(getKey: T => K, getValue: T => Double, getDefault: T => StatCounter) {

  val statCountersByKey = mutable.Map[K, StatCounter]()
  items.foreach { item =>
    statCountersByKey.getOrElseUpdate(getKey(item), new StatCounter()).merge(getValue(item))
  }

  def getStatCounter(item: T): StatCounter = {
    val statCounter = statCountersByKey.get(getKey(item)) match {
      case Some(statCounter) => statCounter
      case None              => getDefault(item)
    }
    statCounter
  }
}