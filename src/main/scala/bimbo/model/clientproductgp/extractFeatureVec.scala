package bimbo.model.clientproductgp

import bimbo.data.Item
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

object extractFeatureVec {
  
  def apply(item:Item):DenseVector[Double] = {
    DenseVector(1d)
  }
  
  def apply(items:Seq[Item]):DenseMatrix[Double] = {
    DenseVector.horzcat(items.map(i => extractFeatureVec(i)) :_*).t
  }
}