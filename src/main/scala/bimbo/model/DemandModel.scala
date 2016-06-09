package bimbo.model

import breeze.linalg.DenseVector
import bimbo.data.Item

trait DemandModel {
  
   def predict(items:Seq[Item]):DenseVector[Double] 
}