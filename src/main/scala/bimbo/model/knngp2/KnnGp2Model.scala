package bimbo.model.knngp2

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.dao.ItemByProductDAO
import bimbo.model.segmentproduct.util.calcNewProductMap
import bimbo.model.knngp2.util.FeatureVectorFactory
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.numerics._
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.stats._
import dk.gp.gpr.GprModel
import dk.gp.gpr.GprPredictEngine
import bimbo.model.knngp2.util.ItemClusterBuilder
import java.util.concurrent.atomic.AtomicInteger
import bimbo.model.knngp2.knn.CoverTreeKnn
import bimbo.model.knngp2.knn.LinearKnn
import bimbo.model.knngp2.knn.LinearKnn
import bimbo.model.knngp2.knn.LinearKnn
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel

case class KnnGp2Model(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel {

 
  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId).toArray
    
    val demand = knnGpPredict(trainProductItems,testProductItems,avgLogWeeklySaleDAO)
    demand
   

  }

 
}