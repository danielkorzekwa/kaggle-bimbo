package bimbo.model.segmentproduct

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import dk.gp.cov.CovNoise
import dk.gp.cov.CovSEiso

case class SegmentProductCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()
  val covNoise = CovNoise()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val logSaleCov = covSEIso.cov(x1(::, 0 to 0), x2(::, 0 to 0), covFuncParams(0 to 1))
    val clientIdCov = covNoise.cov(x1(::, 1 to 1), x2(::, 1 to 1), covFuncParams(2 to 2))
    val depotIdCov = covNoise.cov(x1(::, 2 to 2), x2(::, 2 to 2), covFuncParams(3 to 3))
    val channelId = covNoise.cov(x1(::, 3 to 3), x2(::, 3 to 3), covFuncParams(3 to 3))
    val routeId = covNoise.cov(x1(::, 4 to 4), x2(::, 4 to 4), covFuncParams(3 to 3))
    val cov = depotIdCov + logSaleCov + clientIdCov + channelId + routeId

    cov
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    ???
  }
}