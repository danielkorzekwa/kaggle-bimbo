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
  
  def d(p1: KnnPoint, p2: KnnPoint): Double = {
   
      val d = DenseVector.fill(5)(0.0)


      d(0) = p1.x(0) - p2.x(0)
      d(1) = if (abs(p1.x(1) - p2.x(1)) > 1e-16) 1.0 else 0.0
      d(2) = if (abs(p1.x(2) - p2.x(2)) > 1e-16) 1.0 else 0.0
      d(3) = if (abs(p1.x(3) - p2.x(3)) > 1e-16) 1.0 else 0.0
      d(4) = if (abs(p1.x(4) - p2.x(4)) > 1e-16) 1.0 else 0.0
      //   d(5) = if (abs(x1(5)- x2(5)) > 1e-16) 1.0 else 0.0

      sum(pow(d, 2) / (ell * ell))
  }
}
