package bimbo.model.knngp2.knn

import bimbo.data.Item
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import bimbo.model.knngp2.util.FeatureVectorFactory

case class BruteKnn(trainSet: Array[Item], covFunc: CovFunc, covFuncParams: DenseVector[Double], featureVectorFactory: FeatureVectorFactory) {

  val xTrain = featureVectorFactory.create(trainSet)

  def getKNN(item: Item, k: Int): Seq[Item] = {

    val x = featureVectorFactory.create(item).toDenseMatrix

    val xTrainSortedIndexes = covFunc.cov(x, xTrain, covFuncParams).toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2).take(k)

    val trainKNNSet = xTrainSortedIndexes.map(index => trainSet(index))
    trainKNNSet
  }
}