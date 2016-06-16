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
import bimbo.data.dao.AvgLogWeeklySaleDAO
import dk.gp.util.saveObject
import java.util.concurrent.atomic.AtomicInteger

object TrainProductGPModelApp extends LazyLogging {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
  val itemDAO = ItemDAO(allItemsDAO)

  val avgLogWeeklySaleDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  def main(args: Array[String]): Unit = {

    logger.info("Getting product ids for training...")
    val productIds = itemDAO.getProductIds().filter(productId => itemDAO.getProductItems(productId).size<500)

    val i = new AtomicInteger(1)
    val gprParamsByProductId: Map[Int, (Array[Double], Double)] = productIds.par.map {
     productId =>

        logger.info("Training model %d/%d".format(i.getAndIncrement, productIds.size))
        val (trainedCovParams, trainedNoiseLogStdDev) = trainGprModel(productId, itemDAO, avgLogWeeklySaleDAO)
        productId -> (trainedCovParams, trainedNoiseLogStdDev)
    }.toList.toMap

    saveObject(gprParamsByProductId, "target/productGPModelParams.kryo")

  }

  private def trainGprModel(productId: Int, itemDAO: ItemDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO): (Array[Double], Double) = {
    val items = itemDAO.getProductItems(productId)

    val x = extractFeatureVec(items, avgLogWeeklySaleDAO)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val gprModel = gpr(x, y, ProductCovFunc(), DenseVector(log(1), log(1), log(1)), log(1))

    logger.info("Trained covFuncParams=%s, noiseLogStdDev=%f".format(gprModel.covFuncParams, gprModel.noiseLogStdDev))

    (gprModel.covFuncParams.toArray, gprModel.noiseLogStdDev)
  }
}