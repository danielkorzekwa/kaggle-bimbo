package bimbo.model.knngp2.knn

import bimbo.data.Item
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import bimbo.model.knngp2.util.FeatureVectorFactory
import dk.gp.math.sqDist
import breeze.linalg.norm
import breeze.numerics._
import breeze.linalg._
case class BruteKnn(trainSet: Array[Item], covFunc: CovFunc, covFuncParams: DenseVector[Double], featureVectorFactory: FeatureVectorFactory) {

  val xTrain = featureVectorFactory.create(trainSet)

  def getKNN(item: Item, k: Int): Seq[Item] = {

    val x1 = featureVectorFactory.create(item)

   val d = DenseVector.tabulate(xTrain.rows){i =>
      
      val d = DenseVector.fill(2)(0.0)
      
      val x2 = xTrain(i,::).t
      
      d(0) = x1(0)- x2(0)
      d(1) = if (abs(x1(1)- x2(1)) > 1e-16) 0.0 else 1.0
//       d(2) = if (abs(x1(2)- x2(2)) > 1e-16) 0.0 else 1.0
//        d(3) = if (abs(x1(3)- x2(3)) > 1e-16) 0.0 else 1.0
//         d(4) = if (abs(x1(4)- x2(4)) > 1e-16) 0.0 else 1.0
//       d(5) = if (abs(x1(5)- x2(5)) > 1e-16) 0.0 else 1.0
     sum(pow(d,2))
    }
    
      val xTrainSortedIndexes2 = d.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 < b._1).map(_._2).take(k)
    val xTrainSortedIndexes = covFunc.cov(x1.toDenseMatrix, xTrain, covFuncParams).toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2).take(k)

    if(!xTrainSortedIndexes2.take(5).equals(xTrainSortedIndexes.take(5))) {
    println(xTrainSortedIndexes2.take(5) + ":" + xTrainSortedIndexes.take(5))
      }
    val trainKNNSet = xTrainSortedIndexes.map(index => trainSet(index))
    trainKNNSet
  }
}