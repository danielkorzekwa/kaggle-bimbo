package bimbo.model.clientproductgp.priordemand

import java.io.File
import bimbo.data.Item
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.GprModel
import dk.gp.cov.CovSEiso
import dk.gp.gpr.predict
import bimbo.data.dao.AvgLogWeeklySaleDAO

/**
 * Predicts demand from client average sales
 */
case class PriorLogDemandModel(items: Seq[Item], avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) {

  private val priorLogDemand = if (items.size == 0) log(7d + 1) else {
    items.map(i => log(i.demand + 1)).sum / items.size
  }

  lazy val gprModel = {
    val x = DenseMatrix(items.map(i => avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(i.clientId).get).toArray.take(1000)).t
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray.take(1000))

    GprModel(x, y, CovSEiso(), DenseVector(log(1), log(1)), log(1))
  }

  def predictLogDemand(item: Item): Double = {

    val priorLogDemand2 = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId) match {
      case Some(avgLogWeeklySale) if (items.size > 0) => predict(DenseMatrix(avgLogWeeklySale), gprModel)(0, 0)
      case _ => priorLogDemand
    }
    priorLogDemand2
  }
}