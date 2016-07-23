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

case class KnnARDCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()
  val covNoise = CovNoise()
  val covSEARD = CovSEARDiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val lellLogSale = exp(2 * covFuncParams(1))
    val ellClientId = exp(2 * covFuncParams(2))
    val ellDepotId = exp(2 * covFuncParams(3))
    val ellChannelId = exp(2 * covFuncParams(4))
    val ellRouteId = exp(2 * covFuncParams(5))
    val ellWeekId = exp(2 * covFuncParams(6))

    val distMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dLogSale = pow(x1(i, 0) - x2(j, 0), 2)
      val dClientId = if (x1(i, 1) == x2(j, 1)) 0d else 1.0
      val dDepotId = if (x1(i, 2) == x2(j, 2)) 0d else 1.0
      val dChannelId = if (x1(i, 3) == x2(j, 3)) 0d else 1.0
      val dRouteId = if (x1(i, 4) == x2(j, 4)) 0d else 1.0
      val dWeekId = pow(x1(i, 6) - x2(j, 6), 2)

      dLogSale / lellLogSale + dClientId / ellClientId + dDepotId / ellDepotId + dChannelId / ellChannelId + dRouteId / ellRouteId +
        dWeekId / ellWeekId
    }

    val cov = covSEARD.cov(distMat, covFuncParams(0))

    cov
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {

    val lellLogSale = exp(2 * covFuncParams(1))
    val ellClientId = exp(2 * covFuncParams(2))
    val ellDepotId = exp(2 * covFuncParams(3))
    val ellChannelId = exp(2 * covFuncParams(4))
    val ellRouteId = exp(2 * covFuncParams(5))
    val ellWeekId = exp(2 * covFuncParams(6))

    val dLogSaleMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dLogSale = pow(x1(i, 0) - x2(j, 0), 2)
      dLogSale / lellLogSale
    }
    val dClientIdMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dClientId = if (x1(i, 1) == x2(j, 1)) 0d else 1.0
      dClientId / ellClientId
    }
    val dDepotIdMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dDepotId = if (x1(i, 2) == x2(j, 2)) 0d else 1.0
      dDepotId / ellDepotId
    }

    val dChannelIdMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dChannelId = if (x1(i, 3) == x2(j, 3)) 0d else 1.0
      dChannelId / ellChannelId
    }

    val dRouteIdMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dRouteId = if (x1(i, 4) == x2(j, 4)) 0d else 1.0
      dRouteId / ellRouteId
    }

    val dWeekIdMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dWeekId = pow(x1(i, 6) - x2(j, 6), 2)
      dWeekId / ellWeekId
    }

    val sqDistMatArray = List(dLogSaleMat, dClientIdMat, dDepotIdMat, dChannelIdMat, dRouteIdMat, dWeekIdMat)
    val covD = covSEARD.covD(sqDistMatArray, covFuncParams(0))
   // covD(6) := 0d
    covD
  }
}