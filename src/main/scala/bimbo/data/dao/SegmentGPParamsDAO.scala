package bimbo.data.dao

import breeze.linalg.DenseVector
import dk.gp.util.loadObject

case class SegmentGPParamsDAO(segmentGPModelParamsFile:String) {
  
   private val modelParamsBySegmentId = loadObject[Map[Int, (Array[Double], Double)]](segmentGPModelParamsFile)

  
  def getSegmentGPParams(segmentId:Int):(DenseVector[Double], Double) = {
   val (covFuncParams,noiseLogStdDev) = modelParamsBySegmentId(segmentId)
   (DenseVector(covFuncParams),noiseLogStdDev)
  }
}