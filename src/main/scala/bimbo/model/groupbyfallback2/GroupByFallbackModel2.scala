package bimbo.model.groupbyfallback2

import scala.collection.Map
import scala.collection.Seq
import bimbo.data.Item
import bimbo.data.ItemDAO
import bimbo.model.DemandModel
import breeze.linalg.DenseVector
import breeze.numerics._
import org.apache.spark.util.StatCounter
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging

case class GroupByFallbackModel2[T1](getKey: Item => T1, trainItemDAO: ItemDAO) extends DemandModel with LazyLogging {

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

    val avgLogDemandByKey = mutable.Map[T1, StatCounter]()
    trainProductItems.foreach { item =>
      avgLogDemandByKey.getOrElseUpdate(getKey(item), new StatCounter()).merge(log(item.demand + 1))
    }

    val predictedProductDemand = productItems.map { i =>

      val logDemand = avgLogDemandByKey.get(getKey(i)) match {
        case Some(statCounter) => statCounter.mean
        case None              => productMeanLogDemand
      }

      val demand = exp(logDemand) - 1

      (i, demand)
    }

    predictedProductDemand
  }

}
