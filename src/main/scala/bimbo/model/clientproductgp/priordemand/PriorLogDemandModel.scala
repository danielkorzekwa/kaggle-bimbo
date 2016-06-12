package bimbo.model.clientproductgp.priordemand

import java.io.File
import bimbo.data.Item
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.GprModel
import dk.gp.cov.CovSEiso
import dk.gp.gpr.predict

/**
 * Predicts demand from client average sales
 */
case class PriorLogDemandModel(items: Seq[Item]) {

  val clientAvgLogWeeklySale = csvread(new File("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale.csv"), skipLines = 1)
  val avgLogWeeklySaleByClient: Map[Int, Double] = (0 until clientAvgLogWeeklySale.rows).map(i => clientAvgLogWeeklySale(i, 0).toInt -> clientAvgLogWeeklySale(i, 1)).toMap

  private val priorLogDemand = if (items.size == 0) log(7d + 1) else {
    items.map(i => log(i.demand + 1)).sum / items.size
  }

  val x = DenseMatrix(items.map(i => avgLogWeeklySaleByClient(i.clientId)).toArray.take(1000)).t
  val y = DenseVector(items.map(i => log(i.demand + 1)).toArray.take(1000))

  val gprModel = GprModel(x, y, CovSEiso(), DenseVector(log(1), log(1)), log(1))

  def predictLogDemand(item: Item): Double = {

    val priorLogDemand2 = avgLogWeeklySaleByClient.get(item.clientId) match {
      case Some(avgLogWeeklySale) => predict(DenseMatrix(avgLogWeeklySale), gprModel)(0, 0)
      case None                   => priorLogDemand
    }
    priorLogDemand2
  }
}