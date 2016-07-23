package bimbo.model.depot

import breeze.linalg.DenseMatrix
import smile.math.distance.Metric
import dk.gp.cov.CovFunc
import breeze.linalg.DenseVector
import breeze.linalg.norm
import breeze.numerics._
import breeze.linalg._
import bimbo.model.knngp2.knn.KnnPoint

case class ItemDistanceDepot(covFuncParams: DenseVector[Double]) extends Metric[KnnPoint] {

  val itemDistanceCalc = ItemDistanceCalc(covFuncParams)
 
  def d(p1: KnnPoint, p2: KnnPoint): Double = itemDistanceCalc.calcDistance(p1.x, p2.x) 

}
