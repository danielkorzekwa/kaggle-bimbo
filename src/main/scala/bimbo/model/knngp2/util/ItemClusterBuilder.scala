package bimbo.model.knngp2.util

import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection.mutable.ListBuffer
import scala.collection._
import scala.util.Random
import bimbo.data.Item
import breeze.linalg.DenseVector
import bimbo.data.dao.AvgLogWeeklySaleDAO
import dk.gp.cov.CovFunc

//clustering scheme
//http://www.kyb.mpg.de/fileadmin/user_upload/files/publications/attachments/Nguyen-Tuong-ModelLearningLocalGaussianl_6067%5b0%5d.pdf
case class ItemClusterBuilder(itemCovFunc: CovFunc, covFuncParams: DenseVector[Double], threshold: Double, 
    featureVectorFactory:FeatureVectorFactory)
    extends LazyLogging {

  private var itemClusters = mutable.Map[Item, ListBuffer[Item]]()

  def getItemClusters() = itemClusters
  
  def processItem(item: Item) = {

    if (itemClusters.isEmpty) {
      itemClusters += item -> ListBuffer(item)
    } else {

      val (cluster, distance) = getNearestClusters(item).head

      if (distance > threshold) itemClusters(cluster) += item
      else {
        logger.info("Number of clusters:" + itemClusters.size)
        itemClusters.getOrElseUpdate(item, ListBuffer[Item]()) += item
      }
    }

  }

  def getNearestClusters(item: Item): Seq[(Item, Double)] = {
    val nearestClusters = itemClusters.keys.map { cluster =>

      val itemFeatureVec = featureVectorFactory.create(item)

      val clusterFeatureVec = featureVectorFactory.create(cluster)
      val covValue = itemCovFunc.cov(itemFeatureVec.toDenseMatrix, clusterFeatureVec.toDenseMatrix, covFuncParams)(0, 0)

      cluster -> covValue
//    }.toList.sortBy(x => x._2).last
 }.toList.sortWith((a,b) => a._2>b._2)
   nearestClusters
  }
  def getNNearestItems(item:Item,n:Int):Seq[Item] = {
    val nearestClusters = getNearestClusters(item)
    
    val nearestItems = ListBuffer[Item]()
    
    nearestClusters.foreach{ case (cluster,covVal) =>
      if(nearestItems.size<n) {
      
        nearestItems ++= itemClusters(cluster) 
      }
    }
    nearestItems.toList
  }

  def getClusters(): Map[Item, Seq[Item]] = itemClusters.toMap

}