package bimbo.model.clientproductgp.priordemand

import breeze.linalg.DenseMatrix
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.util.StatCounterByKey
import breeze.linalg.DenseVector
import bimbo.data.Item
import breeze.numerics._
import bimbo.data.dao.AvgLogDemandByClientDAO

object createAvgLogDemandData {

  def apply(items: Seq[Item], avgLogDemandDAO: AvgLogDemandByClientDAO): (DenseMatrix[Double], DenseVector[Double]) = {

    val xy: Seq[(Double, Double)] = items.map { item =>
      val avgLogDemand = avgLogDemandDAO.getAvgLogDemand(item.clientId).get
      val demand = log(item.demand + 1)
      (avgLogDemand, demand)
    }
    val demandByAvgLogDemandStatCounter = StatCounterByKey(xy)(
      getKey = item => item._1,
      getValue = item => item._2,
      getDefault = item => None)

    val x = DenseMatrix(demandByAvgLogDemandStatCounter.getStatCountersByKey.keys.toArray).t
    val y = DenseVector(demandByAvgLogDemandStatCounter.getStatCountersByKey.keys.toArray.map(avgLogDemand => demandByAvgLogDemandStatCounter.getStatCountersByKey(avgLogDemand).mean).toArray)
    (x, y)

  }
}