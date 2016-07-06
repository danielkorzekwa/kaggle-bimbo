package bimbo.model.knngp2

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics.log
import dk.gp.cov.CovFunc
import dk.gp.cov.CovNoise
import dk.gp.cov.CovSEiso
import breeze.numerics._

case class KnnGP2CovFunc2() extends CovFunc {

  val covSEIso = CovSEiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val distMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val d0 = pow(x1(i, 0) - x2(j, 0), 2)
      val d1 = if (x1(i, 1) == x2(j, 1)) 0d else 1.0
      val d2 = if (x1(i, 2) == x2(j, 2)) 0d else 1.0
      val d3 = if (x1(i, 3) == x2(j, 3)) 0d else 1.0
      val d4 = if (x1(i, 4) == x2(j, 4)) 0d else 1.0
      (d0 + d1 + d2 + d3 + d4)
    }

    val cov = covSEIso.cov(distMat, covFuncParams)
    cov
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    val distMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val d0 = pow(x1(i, 0) - x2(j, 0), 2)
      val d1 = if (x1(i, 1) == x2(j, 1)) 0d else 1.0
      val d2 = if (x1(i, 2) == x2(j, 2)) 0d else 1.0
      val d3 = if (x1(i, 3) == x2(j, 3)) 0d else 1.0
      val d4 = if (x1(i, 4) == x2(j, 4)) 0d else 1.0
      (d0 + d1 + d2 + d3 + d4)
    }

    val covD = covSEIso.covD(distMat, covFuncParams)
    covD
  }
}