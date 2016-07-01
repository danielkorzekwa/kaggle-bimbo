import breeze.linalg.DenseMatrix
import smile.math.distance.Metric
import dk.gp.cov.CovFunc
import breeze.linalg.DenseVector
import breeze.linalg.norm


case class ItemDistance(covFunc:CovFunc,covFuncParams:DenseVector[Double]) extends Metric[Array[Double]] {
   
  def d(x: Array[Double], y: Array[Double]): Double = {
      val xData = DenseMatrix(x)
      val yData = DenseMatrix(y)
 //   6-  covFunc.cov(xData, yData, covFuncParams)(0, 0)
   
    val d = xData.toDenseVector-yData.toDenseVector
    norm(d,2)
    }  
    }
