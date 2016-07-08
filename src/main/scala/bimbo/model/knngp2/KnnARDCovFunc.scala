package bimbo.model.knngp2

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics.log
import dk.gp.cov.CovFunc
import dk.gp.cov.CovNoise
import dk.gp.cov.CovSEiso

case class KnnARDCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()
  val covNoise = CovNoise()

  val ell = log(1.6)

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val logSf = covFuncParams(0)
    val logEllLogSale = covFuncParams(1)
    val logEllClientId = covFuncParams(2)
    val logEllDepotId = covFuncParams(3)
    val logEllChannelId = covFuncParams(4)
    val logEllRouteId = covFuncParams(5)

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

    val clientIdCov = covSEIso.cov(clientIdCovSqDistMat, DenseVector(log(1), logEllClientId))
    val depotIdCov = covSEIso.cov(depotIdCovSqDistMat, DenseVector(log(1), logEllDepotId))
    val channelId = covSEIso.cov(channelIdCovSqDistMat, DenseVector(log(1), logEllChannelId))
    val routeIdCov = covSEIso.cov(routeIdCovSqDistMat, DenseVector(log(1), logEllRouteId))

    val cov = logSaleCov :* clientIdCov :* depotIdCov :* channelId :* routeIdCov
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

    val clientIdCov = covSEIso.cov(clientIdCovSqDistMat, DenseVector(log(1), logEllClientId))
    val depotIdCov = covSEIso.cov(depotIdCovSqDistMat, DenseVector(log(1), logEllDepotId))
    val channelIdCov = covSEIso.cov(channelIdCovSqDistMat, DenseVector(log(1), logEllChannelId))
    val routeIdCov = covSEIso.cov(routeIdCovSqDistMat, DenseVector(log(1), logEllRouteId))

    val cov = logSaleCov :* clientIdCov :* depotIdCov :* channelIdCov :* routeIdCov

    val logSaleCovD = covSEIso.covD(x1(::, 0 to 0), x2(::, 0 to 0), DenseVector(logSf, logEllLogSale)).map { logSaleCovD => logSaleCovD :*clientIdCov :* depotIdCov :* channelIdCov :* routeIdCov }
    val clientIdCovD = covSEIso.covD(clientIdCovSqDistMat, DenseVector(log(1), logEllClientId)).map(clientIdCovD => logSaleCov :* clientIdCovD :* depotIdCov :* channelIdCov :* routeIdCov)
    val depotIdCovD = covSEIso.covD(depotIdCovSqDistMat, DenseVector(log(1), logEllDepotId)).map(depotIdCovD => logSaleCov :* clientIdCov :* depotIdCovD :* channelIdCov :* routeIdCov)
    val channelIdD = covSEIso.covD(channelIdCovSqDistMat, DenseVector(log(1), logEllChannelId)).map(channelIdD => logSaleCov :* clientIdCov :* depotIdCov :* channelIdD :* routeIdCov)
    val routeIdD = covSEIso.covD(routeIdCovSqDistMat, DenseVector(log(1), logEllRouteId)).map(routeIdD => logSaleCov :* clientIdCov :* depotIdCov :* channelIdCov :* routeIdD)
    logSaleCovD :+ clientIdCovD(1) :+ depotIdCovD(1) :+ channelIdD(1) :+ routeIdD(1)
  }
}