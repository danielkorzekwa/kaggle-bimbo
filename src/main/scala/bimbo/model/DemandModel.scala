package bimbo.model

import breeze.linalg.DenseVector
import bimbo.data.Item
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging

trait DemandModel extends LazyLogging {

  def predict(items: Seq[Item]): DenseVector[Double] = {

    val itemsByProduct = items.groupBy { i => i.productId }

    val i = new AtomicInteger(1)
    val predictedDemandByItem: Map[Item, Double] = itemsByProduct.toList.flatMap {
      case (productId, productItems) =>
        if (i.getAndIncrement % 10 == 0) logger.info("Predicting product %d/%d".format(i.get, itemsByProduct.size))
        predictProductDemand(productId, productItems)
    }.toMap

    val predictedDemand = DenseVector(items.map(i => predictedDemandByItem(i)).toArray)

    predictedDemand
  }

  def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)]
}