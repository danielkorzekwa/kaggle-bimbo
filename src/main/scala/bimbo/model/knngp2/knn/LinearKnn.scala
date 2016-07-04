package bimbo.model.knngp2.knn

import bimbo.data.Item
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import bimbo.model.knngp2.util.FeatureVectorFactory
import smile.neighbor.LinearSearch

case class LinearKnn(trainSet: Array[Item], covFunc: CovFunc, covFuncParams: DenseVector[Double], featureVectorFactory: FeatureVectorFactory) {
  
  
  
   val data = trainSet.map { item =>
      KnnPoint(featureVectorFactory.create(item),item.demand)
    }.toArray 
  
  val model = new LinearSearch(data, ItemDistance(covFunc, covFuncParams))
  model.setIdenticalExcluded(false)
  
   def getKNN(item: Item, k: Int): Seq[KnnPoint] = {
      val point = KnnPoint(featureVectorFactory.create(item),item.demand)
      model.knn(point, k.min(trainSet.size)).map{n =>
        
        if(!n.key.equals(n.value))
        println(n.key.equals(n.value) + ":" + n.key +":" + n.value)
        
        n.key
        }
   }
}