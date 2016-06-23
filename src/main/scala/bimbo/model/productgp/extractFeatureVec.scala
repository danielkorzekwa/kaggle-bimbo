package bimbo.model.productgp

import bimbo.data.Item
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import bimbo.data.dao.AvgLogWeeklySaleDAO

object extractFeatureVec {
  
  def apply(item:Item,clientLogSale:Double):DenseVector[Double] = {
  DenseVector(clientLogSale,item.clientId,item.depotId,item.channelId,item.routeId)
  }
  
  def apply(items:Seq[Item], avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO):DenseMatrix[Double] = {
   
    val itemFeatureVectors = items.map{item =>
      val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).get
      extractFeatureVec(item,clientLogSale)
      }
    DenseVector.horzcat(itemFeatureVectors :_*).t
  }
}