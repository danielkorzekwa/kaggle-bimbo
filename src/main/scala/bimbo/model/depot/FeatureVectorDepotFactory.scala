package bimbo.model.depot

import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.Item
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import bimbo.data.dao.townstate.TownState
import bimbo.data.ProductDetails
import bimbo.data.dao.AvgLogPriceByProductDAO
import bimbo.data.GenericProductDetails
import bimbo.data.PgProductDetails

case class FeatureVectorDepotFactory(avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO, newProductMap: Map[Item, Boolean], townStateMap: Map[Int, TownState],
                                     clientNameMap: Map[Int, Int], productMap: Map[Int, ProductDetails], avgLogPriceDAO: AvgLogPriceByProductDAO) {

  def create(items: Seq[Item]): DenseMatrix[Double] = {
    val itemFeatureVectors = items.map { item =>
      create(item)
    }
    DenseVector.horzcat(itemFeatureVectors: _*).t
  }

  def create(item: Item): DenseVector[Double] = {
    val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(5.54149)
    val avgLogPrice = avgLogPriceDAO.getAvgLogPrice(item.productId).getOrElse(2.6)
    create(item, clientLogSale, avgLogPrice)
  }

  private def create(item: Item, clientLogSale: Double, avgLogPrice: Double): DenseVector[Double] = {

    val productDetails = productMap(item.productId)
    val productDetailsHashCode = productDetails.hashCode()
//    val productWeigth = productDetails match {
//      case productDetails:GenericProductDetails => 0d
//      case productDetails:PgProductDetails => productDetails.g
//    }
    
    DenseVector(clientLogSale, item.clientId, item.depotId, item.channelId, item.routeId, productDetailsHashCode, avgLogPrice)
  }
}