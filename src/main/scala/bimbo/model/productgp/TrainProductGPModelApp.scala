package bimbo.model.productgp

import bimbo.data.dao.ItemDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.numerics._
import breeze.linalg.DenseVector
import dk.gp.gpr.gpr
import dk.gp.cov.CovSEiso
import com.typesafe.scalalogging.slf4j.LazyLogging

object TrainProductGPModelApp extends LazyLogging{
  
  def main(args: Array[String]): Unit = {
    
     val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
    val itemDAO = ItemDAO(allItemsDAO)
    
     val avgLogWeeklySaleDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

    
       val items = itemDAO.getProductItems( 32295)
    
        val x = extractFeatureVec(items, avgLogWeeklySaleDAO)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val gprModel = gpr(x, y, ProductCovFunc(), DenseVector(log(1), log(1),log(1)), log(1))
       
    logger.info("Trained covFuncParams=%s, noiseLogStdDev=%f".format(gprModel.covFuncParams,gprModel.noiseLogStdDev))
  }
}