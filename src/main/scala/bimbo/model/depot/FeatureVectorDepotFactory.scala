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
import breeze.numerics._

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
    val avgLogPrice = avgLogPriceDAO.getAvgLogPrice(item.productId).getOrElse(2.61)//.getOrElse(2.19)
    create(item, clientLogSale, avgLogPrice)
  }

  private def create(item: Item, clientLogSale: Double, avgLogPrice: Double): DenseVector[Double] = {

    val productDetails = productMap(item.productId)
    val productDetailsHashCode = productDetails.hashCode()
    val productWeigth = productDetails match {
      case productDetails:GenericProductDetails => log(10000+1)
      case productDetails:PgProductDetails => log(productDetails.g+1)
    }
     val productShortName = productDetails match {
      case productDetails:GenericProductDetails => productDetails.description.split(" ")(0)
      case productDetails:PgProductDetails => productDetails.name.split(" ")(0)
    }
     
    DenseVector(clientLogSale, item.clientId, item.depotId, item.channelId, item.routeId, productDetailsHashCode, avgLogPrice,productWeigth,productShortName.hashCode())
  }
}