package bimbo.model.knngp2.knn

import breeze.linalg.norm
import breeze.linalg.DenseVector

class ItemDistanceKnnGp2 {
  
  def distance(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    
       val d = x-y
    norm(d,2)
  }
}