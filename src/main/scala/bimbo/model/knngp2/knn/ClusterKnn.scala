package bimbo.model.knngp2.knn

import bimbo.data.Item
import bimbo.model.knngp2.util.ItemClusterBuilder
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import bimbo.model.knngp2.util.FeatureVectorFactory

case class ClusterKnn(trainSet: Array[Item], covFunc: CovFunc, covFuncParams: DenseVector[Double], featureVectorFactory: FeatureVectorFactory) extends Knn {

  val itemClusterBuilder = ItemClusterBuilder(covFunc, covFuncParams, threshold = 0.2 * 6, featureVectorFactory)

  trainSet.foreach { item =>
    itemClusterBuilder.processItem(item)
  }
  println("num of clusters=" + itemClusterBuilder.getClusters().size)
  itemClusterBuilder.getClusters().toList.sortBy(_._2.size).foreach { case (item, items) => println(items.size) }

  def getKNN(item: Item, k: Int): Seq[Item] = {

   val clusters = itemClusterBuilder.getNearestClusters(item).filter{ case (cluster,covVal) =>
      
  //    println(covVal + ":" + (0.2*6) + ":" + (covVal > (0.2*6)))
       covVal > (0.1*6)
      
    }.map(_._1)
    
   var nnItems =  clusters.flatMap(cluster => itemClusterBuilder.getClusters()(cluster)).toArray
  //  println(clusters.size + ":" + nnItems.size)
   
    if(nnItems.size==0) {
      println("nnitems size is zero")
      nnItems = trainSet
    }
    
    
   // val nnItems = itemClusterBuilder.getNNearestItems(item, 200).toArray
    val xTrain = featureVectorFactory.create(nnItems)

    val x = featureVectorFactory.create(item).toDenseMatrix

    val xTrainSortedIndexes = covFunc.cov(x, xTrain, covFuncParams).toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2).take(k)

    val trainKNNSet = xTrainSortedIndexes.map(index => nnItems(index))
    trainKNNSet

  }
}