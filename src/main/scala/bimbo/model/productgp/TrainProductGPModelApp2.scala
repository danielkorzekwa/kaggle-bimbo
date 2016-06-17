package bimbo.model.productgp

import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import breeze.linalg.DenseVector
import breeze.numerics.log
import dk.gp.gpr.gpr
import dk.gp.util.saveObject
import bimbo.data.dao.ItemSegmentDAO
import bimbo.data.Item
import dk.gp.util.loadObject

object TrainProductGPModelApp2 extends LazyLogging {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
  val itemDAO = ItemByProductDAO(allItemsDAO)

  val avgLogWeeklySaleDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  logger.info("Creating itemSegmentDAO")
  val itemSegmentDAO = ItemSegmentDAO("target/productClientBySegment.kryo")

  val modelParamsBySegmentId = loadObject[Map[Int, (Array[Double], Double)]]("target/segmentGPModelParams.kryo")

  def main(args: Array[String]): Unit = {

    logger.info("Getting product ids for training...")
    val productIds = List(40930) //itemDAO.getProductIds().filter(productId => itemDAO.getProductItems(productId).size < 500)
    val trainItems = productIds.flatMap(productId => itemDAO.getProductItems(productId))
    val segmentIds = productIds.flatMap(productId => itemDAO.getProductItems(productId).map(item => itemSegmentDAO.getSegment(item))).distinct
    val i = new AtomicInteger(1)
    val gprParamsBySegmentId: Map[Int, (Array[Double], Double)] = segmentIds.par.map {
      segmentId =>

        val trainedModelParams = modelParamsBySegmentId.get(segmentId) match {
          case Some(modelParams) => modelParams
          case None => {

            logger.info("Training model %d/%d,segmentId=%d".format(i.getAndIncrement, segmentIds.size, segmentId))
            val (trainedCovParams, trainedNoiseLogStdDev) = try {
              trainGprModel(segmentId, trainItems, itemDAO, avgLogWeeklySaleDAO)
            } catch {
              case e: Exception => {
                logger.info("Training model for segment %d failed".format(segmentId))
                throw e
              }
            }

            (trainedCovParams, trainedNoiseLogStdDev)

          }
        }

        segmentId -> trainedModelParams
    }.toList.toMap

    saveObject(gprParamsBySegmentId, "target/segmentGPModelParams.kryo")

  }

  private def trainGprModel(segmentId: Int, trainItems: Seq[Item], itemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO): (Array[Double], Double) = {
    val segmentItems = trainItems.filter(item => itemSegmentDAO.getSegment(item) == segmentId)

    val x = extractFeatureVec(segmentItems, avgLogWeeklySaleDAO)
    val y = DenseVector(segmentItems.map(i => log(i.demand + 1)).toArray)

    val gprModel = gpr(x, y, ProductCovFunc(), DenseVector(log(1), log(1), log(1)), log(1))

    logger.info("Trained covFuncParams=%s, noiseLogStdDev=%f".format(gprModel.covFuncParams, gprModel.noiseLogStdDev))

    (gprModel.covFuncParams.toArray, gprModel.noiseLogStdDev)
  }
}