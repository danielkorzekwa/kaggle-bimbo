package bimbo.model.clientproductgp

import bimbo.data.Item
import breeze.numerics._

object calcProductMeanLogDemand {

  def apply(items: Seq[Item]): Double = {
    if (items.size == 0) log(7d + 1) else {
      items.map(i => log(i.demand + 1)).sum / items.size
    }
  }
}