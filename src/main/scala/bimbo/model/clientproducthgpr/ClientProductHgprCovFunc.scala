package bimbo.model.clientproducthgpr

import dk.gp.cov.CovFunc
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovNoise
import dk.gp.cov.CovSEiso

case class ClientProductHgprCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()
  val covNoise = CovNoise()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

 //   val depotIdCov = covNoise.cov(x1(::, 0 to 0), x2(::, 0 to 0), covFuncParams(0 to 0))
    val clientIdCov = covNoise.cov(x1(::, 1 to 1), x2(::, 1 to 1), covFuncParams(0 to 0))
    val logSaleCov = covSEIso.cov(x1(::, 2 to 2), x2(::, 2 to 2), covFuncParams(1 to 2))
     clientIdCov + logSaleCov
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = ???

}