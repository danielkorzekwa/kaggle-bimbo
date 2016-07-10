package bimbo.model.depot
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics.log
import dk.gp.cov.CovFunc
import dk.gp.cov.CovNoise
import dk.gp.cov.CovSEiso
import breeze.numerics._

case class DepotARDCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val logSf = covFuncParams(0)
    val logEllLogSale = covFuncParams(1)
    val logEllClientId = covFuncParams(2)
    val logEllDepotId = covFuncParams(3)
    val logEllChannelId = covFuncParams(4)
    val logEllRouteId = covFuncParams(5)
    val logEllProductId = covFuncParams(6)
    val logEllLogAvgPrice = covFuncParams(7)

    val logSaleCov = covSEIso.cov(x1(::, 0 to 0), x2(::, 0 to 0), DenseVector(logSf, logEllLogSale))

    val clientIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 1) == x2(j, 1)) 0d else 1.0
    }

    val depotIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 2) == x2(j, 2)) 0d else 1.0
    }

    val channelIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 3) == x2(j, 3)) 0d else 1.0
    }

    val routeIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 4) == x2(j, 4)) 0d else 1.0
    }

    val productIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 5) == x2(j, 5)) 0d else 1.0
    }

    val clientIdCov = covSEIso.cov(clientIdCovSqDistMat, DenseVector(log(1), logEllClientId))
    val depotIdCov = covSEIso.cov(depotIdCovSqDistMat, DenseVector(log(1), logEllDepotId))
    val channelIdCov = covSEIso.cov(channelIdCovSqDistMat, DenseVector(log(1), logEllChannelId))
    val routeIdCov = covSEIso.cov(routeIdCovSqDistMat, DenseVector(log(1), logEllRouteId))
    val productIdCov = covSEIso.cov(productIdCovSqDistMat, DenseVector(log(1), logEllProductId))

    val logAvgPriceCov = covSEIso.cov(x1(::, 6 to 6), x2(::, 6 to 6), DenseVector(log(1), logEllLogAvgPrice))

    val cov = logSaleCov :* clientIdCov :* depotIdCov :* channelIdCov :* routeIdCov :* productIdCov //:* logAvgPriceCov
    // cov
    cov
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    val logSf = covFuncParams(0)
    val logEllLogSale = covFuncParams(1)
    val logEllClientId = covFuncParams(2)
    val logEllDepotId = covFuncParams(3)
    val logEllChannelId = covFuncParams(4)
    val logEllRouteId = covFuncParams(5)
    val logEllProductId = covFuncParams(6)
    val logEllLogAvgPrice = covFuncParams(7)

    val logSaleCov = covSEIso.cov(x1(::, 0 to 0), x2(::, 0 to 0), DenseVector(logSf, logEllLogSale))

    val clientIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 1) == x2(j, 1)) 0d else 1.0
    }

    val depotIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 2) == x2(j, 2)) 0d else 1.0
    }

    val channelIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 3) == x2(j, 3)) 0d else 1.0
    }

    val routeIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 4) == x2(j, 4)) 0d else 1.0
    }

    val productIdCovSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 5) == x2(j, 5)) 0d else 1.0
    }

    val clientIdCov = covSEIso.cov(clientIdCovSqDistMat, DenseVector(log(1), logEllClientId))
    val depotIdCov = covSEIso.cov(depotIdCovSqDistMat, DenseVector(log(1), logEllDepotId))
    val channelIdCov = covSEIso.cov(channelIdCovSqDistMat, DenseVector(log(1), logEllChannelId))
    val routeIdCov = covSEIso.cov(routeIdCovSqDistMat, DenseVector(log(1), logEllRouteId))
    val productIdCov = covSEIso.cov(productIdCovSqDistMat, DenseVector(log(1), logEllProductId))

    val logAvgPriceCov = covSEIso.cov(x1(::, 6 to 6), x2(::, 6 to 6), DenseVector(log(1), logEllLogAvgPrice))

    val logSaleCovD = covSEIso.covD(x1(::, 0 to 0), x2(::, 0 to 0), DenseVector(logSf, logEllLogSale)).map { logSaleCovD => logSaleCovD :* clientIdCov :* depotIdCov :* channelIdCov :* routeIdCov :* productIdCov :* logAvgPriceCov }
    val clientIdCovD = covSEIso.covD(clientIdCovSqDistMat, DenseVector(log(1), logEllClientId)).map(clientIdCovD => logSaleCov :* clientIdCovD :* depotIdCov :* channelIdCov :* routeIdCov :* productIdCov :* logAvgPriceCov)
    val depotIdCovD = covSEIso.covD(depotIdCovSqDistMat, DenseVector(log(1), logEllDepotId)).map(depotIdCovD => logSaleCov :* clientIdCov :* depotIdCovD :* channelIdCov :* routeIdCov :* productIdCov :* logAvgPriceCov)
    val channelIdD = covSEIso.covD(channelIdCovSqDistMat, DenseVector(log(1), logEllChannelId)).map(channelIdD => logSaleCov :* clientIdCov :* depotIdCov :* channelIdD :* routeIdCov :* productIdCov :* logAvgPriceCov)
    val routeIdD = covSEIso.covD(routeIdCovSqDistMat, DenseVector(log(1), logEllRouteId)).map(routeIdD => logSaleCov :* clientIdCov :* depotIdCov :* channelIdCov :* routeIdD :* productIdCov :* logAvgPriceCov)
    val productIdCovD = covSEIso.covD(productIdCovSqDistMat, DenseVector(log(1), logEllProductId)).map(productIdCovD => logSaleCov :* clientIdCov :* depotIdCov :* channelIdCov :* routeIdCov :* productIdCovD :* logAvgPriceCov)
    val logAvgPriceCovD = covSEIso.covD(x1(::, 6 to 6), x2(::, 6 to 6), DenseVector(log(1), logEllLogAvgPrice)).map(logAvgPriceCovD => logSaleCov :* clientIdCov :* depotIdCov :* channelIdCov :* routeIdCov :* productIdCov :*  logAvgPriceCovD)

    logSaleCovD :+ clientIdCovD(1) :+ depotIdCovD(1) :+ channelIdD(1) :+ routeIdD(1) :+ productIdCovD(1) :+ logAvgPriceCovD(1)
  }
}