package bimbo.model.depot
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics.log
import dk.gp.cov.CovFunc
import dk.gp.cov.CovNoise
import dk.gp.cov.CovSEiso
import dk.gp.cov.CovSEARDiso
import breeze.numerics._
import dk.bayes.math.covfunc.CovSEARDIso

case class DepotARDCovFunc() extends CovFunc {

  val covSEARD = CovSEARDiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val itemDistanceCalc = ItemDistanceCalc(covFuncParams)

    val distMat = itemDistanceCalc.calcDistance(x1, x2)

    val cov = covSEARD.cov(distMat, covFuncParams(0))

    cov
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {

    val itemDistanceCalc = ItemDistanceCalc(covFuncParams)

    val sqDistMatArray = itemDistanceCalc.calcDistanceArray(x1, x2)
    val covD = covSEARD.covD(sqDistMatArray, covFuncParams(0))
    covD
  }
}