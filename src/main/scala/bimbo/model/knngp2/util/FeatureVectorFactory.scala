package bimbo.model.knngp2.util

import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.Item
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import bimbo.data.dao.townstate.TownState

case class FeatureVectorFactory(avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO, newProductMap: Map[Item, Boolean],townStateMap:Map[Int,TownState],
clientNameMap:Map[Int,Int]    
) {

  def create(item: Item): DenseVector[Double] = {
    val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(5.54149)
    create(item, clientLogSale)
  }

  def create(items: Seq[Item]): DenseMatrix[Double] = {
    val itemFeatureVectors = items.map { item =>
      create(item)
    }
    DenseVector.horzcat(itemFeatureVectors: _*).t
  }

  private def create(item: Item, clientLogSale: Double): DenseVector[Double] = {
    val isNewProduct = if(newProductMap(item)) 1.0 else 0.0
    //val townId = townStateMap(item.depotId).townId
     // val stateId = townStateMap(item.depotId).stateId
    val clientNameId = clientNameMap(item.clientId)
    
    DenseVector(clientLogSale, item.clientId, item.depotId, item.channelId, item.routeId,clientNameId)
   // DenseVector(clientLogSale, item.clientId, item.depotId, item.channelId, item.routeId,isNewProduct)
  }
}