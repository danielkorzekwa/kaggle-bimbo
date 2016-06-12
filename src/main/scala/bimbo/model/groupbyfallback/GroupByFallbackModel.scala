package bimbo.model.groupbyfallback

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
import bimbo.util.StatCounterByKey

case class GroupByFallbackModel( trainItemDAO: ItemDAO) extends DemandModel with LazyLogging {

  def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val productMeanLogDemand = if (trainProductItems.size == 0) log(7d + 1) else {
      trainProductItems.map(i => log(i.demand + 1)).sum / trainProductItems.size
    }

    val clientProductStatCounter = StatCounterByKey[Item, (Int, Int)](trainProductItems)(
      getKey = (item: Item) => (item.clientId, item.productId),
      getValue = (item: Item) => log(item.demand + 1),
      getDefault = (item: Item) => StatCounter(productMeanLogDemand))

//      val routeClientProductCounter = StatCounterByKey[Item, (Int, Int,Int)](trainProductItems)(
//      getKey = (item: Item) => (item.routeId,item.clientId, item.productId),
//      getValue = (item: Item) => log(item.demand + 1),
//      getDefault = (item: Item) => clientProductStatCounter.getStatCounter(item))
      
    val predictedProductDemand = productItems.map { item =>
      val logDemand = clientProductStatCounter.getStatCounter(item).mean
      val demand = exp(logDemand) - 1
      (item, demand)
    }

    predictedProductDemand
  }

}
