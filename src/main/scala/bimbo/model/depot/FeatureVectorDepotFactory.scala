package bimbo.model.depot

import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.Item
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import bimbo.data.dao.townstate.TownState
import bimbo.data.ProductDetails

case class FeatureVectorDepotFactory(avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO, newProductMap: Map[Item, Boolean],townStateMap:Map[Int,TownState],
clientNameMap:Map[Int,Int],productMap: Map[Int, ProductDetails]  
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
    
    val productDetailsHashCode = productMap(item.productId).hashCode()
    DenseVector(clientLogSale, item.clientId, item.depotId, item.channelId, item.routeId,productDetailsHashCode)
  }
}