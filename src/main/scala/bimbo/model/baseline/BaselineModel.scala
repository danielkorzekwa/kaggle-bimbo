package bimbo.model.baseline

import bimbo.data.Item
import bimbo.model.DemandModel
import breeze.linalg.DenseVector

case class BaselineModel() extends DemandModel {
  
  def predict(items:Seq[Item]):DenseVector[Double] = {
    DenseVector(items.map(i => 7.0).toArray)
  }
}