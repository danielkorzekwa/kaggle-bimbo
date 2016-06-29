package bimbo.model.segmentproduct.util

import scala.collection.mutable.HashSet
import bimbo.data.Item
import scala.collection._

object calcNewProductMap {

  /**
   * @return key - item, value - true is client product has not been see yet before.
   */
  def apply(items: Seq[Item]): immutable.Map[Item, Boolean] = {

    //Set[(clientId,productId)]
    val observedProductsSet: mutable.Set[(Int, Int)] = HashSet()

    val newProductMap: immutable.Map[Item, Boolean] = items.sortBy { item => item.weekId }.map { item =>

      val clientProductKey = (item.clientId, item.productId)
      val isNewItem = !observedProductsSet.contains((item.clientId, item.productId))

      observedProductsSet += clientProductKey

      (item, isNewItem)
    }.toList.toMap

    newProductMap

  }
}