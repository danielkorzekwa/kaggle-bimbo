package bimbo.model.clientproductgp

import dk.gp.cov.CovFunc
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso

case class RouteCovFunc() extends CovFunc{
  
  val covSEIso = CovSEiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val routeSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 0) == x2(j, 0)) 0d else 1.0
    }

    val covMat = covSEIso.cov(routeSqDistMat, covFuncParams)
    covMat

  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    val routeSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 0) == x2(j, 0)) 0d else 1.0
    }

    val covMatD = covSEIso.covD(routeSqDistMat, covFuncParams)
    covMatD
  }
  
}