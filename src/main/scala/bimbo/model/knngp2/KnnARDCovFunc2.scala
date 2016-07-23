package bimbo.model.knngp2

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics.log
import dk.gp.cov.CovFunc
import dk.gp.cov.CovNoise
import dk.gp.cov.CovSEiso
import breeze.linalg._
import breeze.numerics._
import dk.gp.cov.CovSEARDiso

case class KnnARDCovFunc2() extends CovFunc {

  val covSEIso = CovSEiso()
  val covNoise = CovNoise()
  val covSEARD = CovSEARDiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val lellLogSale = exp(2 * covFuncParams(1))
     val ellWeekId = exp(2 * covFuncParams(2))

    val distMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dLogSale = pow(x1(i, 0) - x2(j, 0), 2)
      val dWeekId = pow(x1(i, 6) - x2(j, 6), 2)

      dLogSale / lellLogSale + dWeekId / ellWeekId
    }

    val cov = covSEARD.cov(distMat, covFuncParams(0))

    cov
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {

    val lellLogSale = exp(2 * covFuncParams(1))
   
    val ellWeekId = exp(2 * covFuncParams(2))

    val dLogSaleMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dLogSale = pow(x1(i, 0) - x2(j, 0), 2)
      dLogSale / lellLogSale
    }
   

    val dWeekIdMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dWeekId = pow(x1(i, 6) - x2(j, 6), 2)
      dWeekId / ellWeekId
    }

    val sqDistMatArray = List(dLogSaleMat,dWeekIdMat)
    val covD = covSEARD.covD(sqDistMatArray, covFuncParams(0))
   // covD(6) := 0d
    covD
  }
}