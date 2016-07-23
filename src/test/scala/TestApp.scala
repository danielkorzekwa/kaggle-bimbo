

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import dk.gp.math.sqDist
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.gpr
import bimbo.data.dao.AvgLogDemandByClientDAO
import dk.gp.gpr.gprPredict
import bimbo.data.PgProductDetails
import breeze.stats._
import bimbo.data.dao.ItemByDepotDAO
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.data.dao.ProductDAO
import bimbo.model.depot.CoverTreeDepot
import bimbo.model.depot.FeatureVectorDepotFactory
import bimbo.model.knngp2.util.calcNewProductMap
import bimbo.data.Item
import bimbo.data.dao.clientname.ClientNameDAO
import bimbo.data.dao.townstate.TownStateDAO
import bimbo.data.dao.AvgLogPriceByProductDAO
import java.util.concurrent.atomic.AtomicInteger

object TestApp extends LazyLogging {

  val initialCovFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1), log(1), log(1), log(1))

  val clientNamesDAO = ClientNamesDAO("/mnt/bimbo/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
  val trainItemDAO = ItemByProductDAO(allItemsDAO)

  val productMap = ProductDAO("/mnt/bimbo/producto_tabla.csv").getProductMap()

  val trainItemByPgProductDAO = ItemByPgProductDAO(allItemsDAO, productMap)
  val trainItemByDepotDAO = ItemByDepotDAO(allItemsDAO)

  val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")
  val avgLogPriceDAO = AvgLogPriceByProductDAO("/mnt/bimbo/stats/avgLogPriceByProduct_3_to_8.csv")

  val townStateMap = TownStateDAO("/mnt/bimbo/town_state.csv").getTownStateMap()
  val clientNameMap = ClientNameDAO("/mnt/bimbo/cliente_tabla.csv").getClientNameIdMap()

  def main(args: Array[String]): Unit = {

  
    val allItems = {
      
        val depotItems = trainItemByDepotDAO.getDepotItems(1127)
 logger.info("Depot items size:" + depotItems.size)
 
    val productIds = depotItems.map(i => i.productId).distinct
    logger.info("Product ids size:" + productIds.size)
    
    val productItems = productIds.flatMap(productId => trainItemDAO.getProductItems(productId))
    logger.info("Product items size:" + productItems.size)
 
    
      
      (depotItems ++ productItems).distinct
    }
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(allItems)
    val featureVectorFactory = FeatureVectorDepotFactory(avgLogWeeklySaleByClientDAO, newProductMap, townStateMap, clientNameMap, productMap, avgLogPriceDAO,null)

   logger.info("Data size:" + allItems.size)
    val knnModel = CoverTreeDepot(allItems.toArray, initialCovFuncParams, featureVectorFactory)

    val d = allItems.size
    val i = new AtomicInteger(0)
    
    allItems.foreach { item => 
        if (i.getAndIncrement % 1000 == 0) logger.info("Predicting depot %d/%d".format(i.get, d))
      knnModel.getKNN(item, 100) 
      }
  }

}