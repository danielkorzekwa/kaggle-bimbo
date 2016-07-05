package bimbo.model.knngp2.knn

import breeze.linalg.DenseMatrix
import smile.math.distance.Metric
import dk.gp.cov.CovFunc
import breeze.linalg.DenseVector
import breeze.linalg.norm
import breeze.numerics._
import breeze.linalg._

case class ItemDistance(covFunc: CovFunc, covFuncParams: DenseVector[Double]) extends Metric[KnnPoint] {

  val logEll = log(1.6)
  val ell = exp(logEll)
  val ellell = ell * ell
  
  val oneByellell = 1.0/ellell
  def d(p1: KnnPoint, p2: KnnPoint): Double = {

   

    
    val d0 = p1.x(0) - p2.x(0)
    val d1 = if (p1.x(1) == p2.x(1)) 0.0 else oneByellell
    val d2 = if (p1.x(2) == p2.x(2)) 0.0 else oneByellell
    val d3 = if (p1.x(3) == p2.x(3)) 0.0 else oneByellell
    val d4 = if (p1.x(4) == p2.x(4)) 0.0 else oneByellell

    (d0 * d0) / ellell + d1 + d2 + d3 + d4
  }

  
}
