package bimbo.model.depot

import breeze.linalg.DenseVector
import breeze.linalg._
import breeze.numerics._
case class ItemDistanceCalc(covFuncParams: DenseVector[Double]) {

  val lellLogSale = exp(2 * covFuncParams(1))
  val ellClientId = exp(2 * covFuncParams(2))
  val ellDepotId = exp(2 * covFuncParams(3))
  val ellChannelId = exp(2 * covFuncParams(4))
  val ellRouteId = exp(2 * covFuncParams(5))
  val ellProductDetailsId = exp(2 * covFuncParams(6))
  val ellLogAvgPrice = exp(2 * covFuncParams(7))
  val ellProductWeigth = exp(2 * covFuncParams(8))
  val ellProductShortName = exp(2 * covFuncParams(9))
val ellProductId = exp(2 * covFuncParams(10))

  def calcDistance(x1: DenseVector[Double], x2: DenseVector[Double]): Double = {

    val dLogSale = pow(x1(0) - x2(0), 2)
    val dClientId = if (x1(1) == x2(1)) 0d else 1.0
    val dDepotId = if (x1(2) == x2(2)) 0d else 1.0
    val dChannelId = if (x1(3) == x2(3)) 0d else 1.0
    val dRouteId = if (x1(4) == x2(4)) 0d else 1.0
    val dProductDetailsId = if (x1(5) == x2(5)) 0d else 1.0
    val dLogAvgPrice = pow(x1(6) - x2(6), 2)
    val productWeightCov = pow(x1(7) - x2(7), 2)
    val dproductShortName = if (x1(8) == x2(8)) 0d else 1.0
    val dProductId = if (x1(9) == x2(9)) 0d else 1.0
    //val dAvgLogDemand = pow(x1(i, 9) - x2(j, 9), 2)

    dLogSale / lellLogSale + dClientId / ellClientId + dDepotId / ellDepotId + dChannelId / ellChannelId + dRouteId / ellRouteId +
      dProductDetailsId / ellProductDetailsId + dLogAvgPrice / ellLogAvgPrice + productWeightCov / ellProductWeigth +
      dproductShortName / ellProductShortName + dProductId/ellProductId
  }

  def calcDistance(x1: DenseMatrix[Double], x2: DenseMatrix[Double]): DenseMatrix[Double] = {
    val distMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      calcDistance(x1(i, ::).t, x2(j, ::).t)
    }

    distMat
  }

  def calcDistanceArray(x1: DenseMatrix[Double], x2: DenseMatrix[Double]): Seq[DenseMatrix[Double]] = {
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

    val dProductDetailsIdMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val dProductDetailsId = if (x1(i, 5) == x2(j, 5)) 0d else 1.0
      dProductDetailsId / ellProductDetailsId
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
    
    val dproductIdCovMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      val productIdov = if (x1(i, 9) == x2(j, 9)) 0d else 1.0

      productIdov / ellProductId
    }

   

    val sqDistMatArray = List(dLogSaleMat, dClientIdMat, dDepotIdMat, dChannelIdMat, dRouteIdMat, dProductDetailsIdMat, dLogAvgPriceMat, dproductWeightCovMat,
      dproductShortNameCovMat,dproductIdCovMat)

    sqDistMatArray
  }
}