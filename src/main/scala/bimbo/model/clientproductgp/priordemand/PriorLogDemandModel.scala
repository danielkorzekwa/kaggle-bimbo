package bimbo.model.clientproductgp.priordemand

import java.io.File
import bimbo.data.Item
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.GprModel
import dk.gp.cov.CovSEiso
import dk.gp.gpr.predict
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.util.StatCounterByKey

/**
 * Predicts demand from client average sales
 */
case class PriorLogDemandModel(items: Seq[Item], avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) {

  private val itemsSize = items.size
  
  private val priorLogDemand = if (items.size == 0) log(7d + 1) else {
    items.map(i => log(i.demand + 1)).sum / items.size
  }

  lazy val gprModel = {
  
    val xy: Seq[(Double, Double)] = items.map { item =>
      val logSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).get
      val demand = log(item.demand + 1)
      (logSale, demand)
    }

    val demandByLogWeeklySaleStatCounter = StatCounterByKey(xy)(
      getKey = item => item._1,
      getValue = item => item._2,
      getDefault = item => None)

      val x2 =  DenseMatrix(demandByLogWeeklySaleStatCounter.getStatCountersByKey.keys.toArray).t
      val y2 = DenseVector(demandByLogWeeklySaleStatCounter.getStatCountersByKey.keys.toArray.map(logSale => demandByLogWeeklySaleStatCounter.getStatCountersByKey(logSale).mean).toArray)
        
    GprModel(x2, y2, CovSEiso(), DenseVector(log(1), log(1)), log(1))
  }

  def predictLogDemand(item: Item): Double = {

    val priorLogDemand2 = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId) match {
      case Some(avgLogWeeklySale) if (itemsSize > 0) => predict(DenseMatrix(avgLogWeeklySale), gprModel)(0, 0)
      case _ => priorLogDemand
    }
    priorLogDemand2
  }
}