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

    val lellLogSale = exp(2 * covFuncParams(1))
    val ellClientId = exp(2 * covFuncParams(2))
    val ellDepotId = exp(2 * covFuncParams(3))
    val ellChannelId = exp(2 * covFuncParams(4))
    val ellRouteId = exp(2 * covFuncParams(5))
    val ellProductId = exp(2 * covFuncParams(6))
    val ellLogAvgPrice = exp(2 * covFuncParams(7))
    val ellProductWeigth = exp(2 * covFuncParams(8))
    val ellProductShortName = exp(2 * covFuncParams(9))

    val distMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dLogSale = pow(x1(i, 0) - x2(j, 0), 2)
      val dClientId = if (x1(i, 1) == x2(j, 1)) 0d else 1.0
      val dDepotId = if (x1(i, 2) == x2(j, 2)) 0d else 1.0
      val dChannelId = if (x1(i, 3) == x2(j, 3)) 0d else 1.0
      val dRouteId = if (x1(i, 4) == x2(j, 4)) 0d else 1.0
      val dProductId = if (x1(i, 5) == x2(j, 5)) 0d else 1.0
      val dLogAvgPrice = pow(x1(i, 6) - x2(j, 6), 2)
      val productWeightCov = pow(x1(i, 7) - x2(j, 7), 2)
 val dproductShortName = if (x1(i, 8) == x2(j, 8)) 0d else 1.0

      dLogSale / lellLogSale + dClientId / ellClientId + dDepotId / ellDepotId + dChannelId / ellChannelId + dRouteId / ellRouteId +
        dProductId / ellProductId + dLogAvgPrice / ellLogAvgPrice + productWeightCov / ellProductWeigth + 
        dproductShortName/ellProductShortName
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
    val ellProductId = exp(2 * covFuncParams(6))
    val ellLogAvgPrice = exp(2 * covFuncParams(7))
    val ellProductWeigth = exp(2 * covFuncParams(8))
 val ellProductShortName = exp(2 * covFuncParams(9))
 
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

    val dProductIdMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dProductId = if (x1(i, 5) == x2(j, 5)) 0d else 1.0
      dProductId / ellProductId
    }

    val dLogAvgPriceMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dLogAvgPrice = pow(x1(i, 6) - x2(j, 6), 2)
      dLogAvgPrice / ellLogAvgPrice
    }

    val dproductWeightCovMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val productWeightCov = pow(x1(i, 7) - x2(j, 7), 2)

      productWeightCov / ellProductWeigth
    }
    
     val dproductShortNameCovMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val productShortNameCov = if (x1(i, 8) == x2(j, 8)) 0d else 1.0

      productShortNameCov / ellProductShortName
    }

    val sqDistMatArray = List(dLogSaleMat, dClientIdMat, dDepotIdMat, dChannelIdMat, dRouteIdMat, dProductIdMat, dLogAvgPriceMat, dproductWeightCovMat,
        dproductShortNameCovMat)
    val covD = covSEARD.covD(sqDistMatArray, covFuncParams(0))
    covD
  }
}