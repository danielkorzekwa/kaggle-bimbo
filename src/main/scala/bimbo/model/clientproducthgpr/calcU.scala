package bimbo.model.clientproducthgpr

import breeze.linalg.DenseMatrix
import breeze.linalg._
import dk.gp.util.calcInducingPointsMatrix
object calcU {

  /**
   * @param x  [depotId,clientId,logStake]
   */
  def apply(x: DenseMatrix[Double]): DenseMatrix[Double] = {

     val depotIdVec = DenseVector(-1d)
    val clientIdVec = DenseVector(-1d)
  
    val logStakeMin = min(x(::, 2))
    val logStakeMax = max(x(::, 2))
    
    val logStakeVec = if(logStakeMin==logStakeMax) DenseVector(logStakeMin) else {
     
      try {
      DenseVector(DenseVector.rangeD(logStakeMin, logStakeMax, (logStakeMax - logStakeMin) / 100).toArray :+ max(x(::, 2)))
      }
      catch {
        case e:Exception => {
           println(x(::, 2).size + ":" + x(::, 2))
           throw e
        }
      }
    }
    
    val u = calcInducingPointsMatrix(Array(depotIdVec,clientIdVec,logStakeVec))
    u
  }

} 