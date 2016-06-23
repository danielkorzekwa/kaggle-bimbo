package bimbo.model.clientproducthgpr

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import bimbo.data.Item
import bimbo.data.dao.AvgLogWeeklySaleDAO

object extractFeatureVec {
  
   
  def apply(item:Item,clientLogSale:Double):DenseVector[Double] = {
     DenseVector(item.depotId,item.clientId,clientLogSale)
  }
  
  def apply(items:Seq[Item],avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO,defaultClientLogSale:Double):DenseMatrix[Double] = {
  
    val featureVecSeq = items.map{item => 
    
        val logSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(defaultClientLogSale)
      extractFeatureVec(item,logSale)
      } 
    DenseVector.horzcat(featureVecSeq :_*).t
  }
}