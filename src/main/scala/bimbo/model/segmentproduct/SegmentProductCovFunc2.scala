package bimbo.model.segmentproduct

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import dk.gp.cov.CovNoise
import dk.gp.cov.CovSEiso
import breeze.numerics._

case class SegmentProductCovFunc2() extends CovFunc {

  val covSEIso = CovSEiso()
  val covNoise = CovNoise()

  val ell=log(1.6)
  
  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val logSaleCov = covSEIso.cov(x1(::, 0 to 0), x2(::, 0 to 0), DenseVector(log(2),ell))
    
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

   
    
    val clientIdCov = covSEIso.cov(clientIdCovSqDistMat,  DenseVector(log(1),ell))
    val depotIdCov = covSEIso.cov(depotIdCovSqDistMat,  DenseVector(log(1),ell))
    val channelId = covSEIso.cov(channelIdCovSqDistMat,  DenseVector(log(1),ell))
    val routeId = covSEIso.cov(routeIdCovSqDistMat,  DenseVector(log(1),ell))
    
    val cov = logSaleCov :* clientIdCov :* depotIdCov :* channelId :* routeId //+ isNewProductCov
   // cov
    cov
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    val logSaleCovD = covSEIso.covD(x1(::, 0 to 0), x2(::, 0 to 0), covFuncParams(0 to 1))
    val clientIdCovD = covNoise.covD(x1(::, 1 to 1), x2(::, 1 to 1), covFuncParams(2 to 2))
    val depotIdCovD = covNoise.covD(x1(::, 2 to 2), x2(::, 2 to 2), covFuncParams(3 to 3))
    val channelIdD = covNoise.covD(x1(::, 3 to 3), x2(::, 3 to 3), covFuncParams(4 to 4))
    val routeIdD = covNoise.covD(x1(::, 4 to 4), x2(::, 4 to 4), covFuncParams(5 to 5))
    val isNewProductCovD = covNoise.covD(x1(::, 5 to 5), x2(::, 5 to 5), covFuncParams(6 to 6))
    logSaleCovD ++clientIdCovD ++ depotIdCovD ++ channelIdD ++ routeIdD ++ isNewProductCovD
  }
}