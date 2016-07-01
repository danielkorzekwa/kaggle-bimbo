package bimbo.model.knngp2.util

import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.Item
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

case class FeatureVectorFactory(avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO, newProductMap: Map[Item, Boolean]) {

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
    DenseVector(clientLogSale, item.clientId, item.depotId, item.channelId, item.routeId,isNewProduct)
  }
}