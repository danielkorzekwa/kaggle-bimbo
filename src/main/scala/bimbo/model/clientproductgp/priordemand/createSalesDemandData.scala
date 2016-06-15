package bimbo.model.clientproductgp.priordemand

import breeze.linalg.DenseMatrix
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.util.StatCounterByKey
import breeze.linalg.DenseVector
import bimbo.data.Item
import breeze.numerics._

object createSalesDemandData {

  def apply(items: Seq[Item], avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO): (DenseMatrix[Double], DenseVector[Double]) = {

    val xy: Seq[(Double, Double)] = items.map { item =>
      val logSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).get
      val demand = log(item.demand + 1)
      (logSale, demand)
    }

    val demandByLogWeeklySaleStatCounter = StatCounterByKey(xy)(
      getKey = item => item._1,
      getValue = item => item._2,
      getDefault = item => None)

    val x = DenseMatrix(demandByLogWeeklySaleStatCounter.getStatCountersByKey.keys.toArray).t
    val y = DenseVector(demandByLogWeeklySaleStatCounter.getStatCountersByKey.keys.toArray.map(logSale => demandByLogWeeklySaleStatCounter.getStatCountersByKey(logSale).mean).toArray)

    (x, y)

  }
}