package bimbo.model.clientproductgp

import dk.gp.cov.CovFunc
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso

case class RouteCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val depotSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 0) == x2(j, 0)) 0d else 1.0
    }

    val routeSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 1) == x2(j, 1)) 0d else 1.0
    }
    
//      val weekSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
//      if (x1(i, 2) == x2(j, 2)) 0d else 1.0
//    }
    val depotCovMat = covSEIso.cov(depotSqDistMat, covFuncParams(0 to 1))
    val routeCovMat = covSEIso.cov(routeSqDistMat, covFuncParams(2 to 3))
  //   val weekCovMat = covSEIso.cov(weekSqDistMat, covFuncParams(4 to 5))
   depotCovMat :* routeCovMat //:* weekCovMat

  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    val depotSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 0) == x2(j, 0)) 0d else 1.0
    }

    val routeSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 1) == x2(j, 1)) 0d else 1.0
    }

//        val weekSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
//      if (x1(i, 2) == x2(j, 2)) 0d else 1.0
//    }
    
    val depotCovMat = covSEIso.cov(depotSqDistMat, covFuncParams(0 to 1))
    val routeCovMat = covSEIso.cov(routeSqDistMat, covFuncParams(2 to 3))
  //val weekCovMat = covSEIso.cov(weekSqDistMat, covFuncParams(4 to 5))
  
    val depotCovDMat = covSEIso.covD(depotSqDistMat, covFuncParams(0 to 1)).map(covD => covD :* routeCovMat )
    val routeCovDMat = covSEIso.covD(routeCovMat, covFuncParams(2 to 3)).map(covD => covD :* depotCovMat )
     // val weekCovDMat = covSEIso.covD(weekCovMat, covFuncParams(4 to 5)).map(covD => covD :* depotCovMat :* routeCovMat)
   
    depotCovDMat ++ routeCovDMat// ++ weekCovDMat

  }

}