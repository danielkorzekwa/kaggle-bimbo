package bimbo.model.baseline

import breeze.linalg.DenseVector
import bimbo.data.TestItem

case class BaselineModel() {
  
  def predict(items:Seq[TestItem]):DenseVector[Double] = {
    DenseVector(items.map(i => 7.0).toArray)
  }
}