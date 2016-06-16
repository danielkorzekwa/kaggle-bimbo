package bimbo.model.productgp

import dk.gp.cov.CovFunc
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import dk.gp.cov.CovNoise

case class ProductCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()
  val covNoise = CovNoise()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val logSaleCov = covSEIso.cov(x1(::, 0 to 0), x2(::, 0 to 0), covFuncParams(0 to 1))
    val clientIdCov = covNoise.cov(x1(::, 1 to 1), x2(::, 1 to 1), covFuncParams(2 to 2))
    logSaleCov + clientIdCov

  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    val logSaleCovD = covSEIso.covD(x1(::, 0 to 0), x2(::, 0 to 0), covFuncParams(0 to 1))
    val clientIdCovD = covNoise.covD(x1(::, 1 to 1), x2(::, 1 to 1), covFuncParams(2 to 2))
    logSaleCovD ++ clientIdCovD
  }
}