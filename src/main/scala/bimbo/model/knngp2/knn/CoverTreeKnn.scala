package bimbo.model.knngp2.knn

import bimbo.data.Item
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import bimbo.model.knngp2.util.FeatureVectorFactory
import smile.neighbor.LinearSearch
import smile.neighbor.CoverTree
import com.typesafe.scalalogging.slf4j.LazyLogging

case class CoverTreeKnn(trainSet: Array[Item], featureVectorFactory: FeatureVectorFactory,covFuncParams:DenseVector[Double]) extends LazyLogging {

  val data = trainSet.map { item =>
    KnnPoint(featureVectorFactory.create(item), item.demand)
  }.toArray

  logger.info("Building cover tree...")
  val model = new CoverTree(data, ItemDistance(covFuncParams))
  model.setIdenticalExcluded(false)
  logger.info("Building cover tree...done")

  def getKNN(item: Item, k: Int): Seq[KnnPoint] = {
    if (trainSet.size <= k) data
    else {

      val point = KnnPoint(featureVectorFactory.create(item), item.demand)
      val p = model.knn(point, k)
      val knnPoints = p.map(_.value)
      if(knnPoints.size ==100) {
      //  println(knnPoints.size)
        knnPoints
      }
      else {
      val linearModel = new LinearSearch(knnPoints, ItemDistance(covFuncParams))
      linearModel.setIdenticalExcluded(false)
      linearModel.knn(point, k).map(_.value)
      }
    }
  }
}