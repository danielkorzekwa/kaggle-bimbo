package bimbo.model.knngp2.knn

import breeze.linalg.DenseMatrix
import smile.math.distance.Metric
import dk.gp.cov.CovFunc
import breeze.linalg.DenseVector
import breeze.linalg.norm
import breeze.numerics._
import breeze.linalg._

case class ItemDistance(covFuncParams: DenseVector[Double]) extends Metric[KnnPoint] {

  val logEllLogSale = exp(2 * covFuncParams(1))
//  val logEllClientId = exp(2 * covFuncParams(2))
//  val logEllDepotId = exp(2 * covFuncParams(3))
//  val logEllChannelId = exp(2 * covFuncParams(4))
//  val logEllRouteId = exp(2 * covFuncParams(5))
  val logEllWeekId = exp(2 * covFuncParams(2))

  def d(p1: KnnPoint, p2: KnnPoint): Double = {

    val d0 = p1.x(0) - p2.x(0)
    //  val d1 = if (p1.x(1) == p2.x(1)) 0.0 else oneByellell
    val d2 = if (p1.x(2) == p2.x(2)) 0.0 else 1
    val d3 = if (p1.x(3) == p2.x(3)) 0.0 else 1
    val d4 = if (p1.x(4) == p2.x(4)) 0.0 else 1
    val dWeekId = p1.x(6) - p2.x(6)
    //(d0 * d0) / logEllLogSale + d2 / logEllDepotId + d3 / logEllChannelId + d4 / logEllRouteId + (dWeekId * dWeekId) / logEllWeekId
      (d0 * d0) / logEllLogSale + (dWeekId * dWeekId) / logEllWeekId
  }

}
