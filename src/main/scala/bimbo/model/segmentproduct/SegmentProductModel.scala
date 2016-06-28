package bimbo.model.segmentproduct

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.dao.ItemByProductDAO
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import breeze.linalg.DenseVector
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.linalg.DenseMatrix
import breeze.numerics._
import breeze.linalg._
import breeze.stats._
import dk.gp.gpr.GprModel
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.gpr.gprPredict
import java.util.concurrent.atomic.AtomicInteger
import dk.gp.gpr.GprPredictEngine
import dk.gp.gpr.GprPredictEngine

case class SegmentProductModel(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel with LazyLogging {

  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    logger.info("Predicting for product: " + productId)

    val trainProductItems = trainItemDAO.getProductItems(productId)
    val trainSize = trainProductItems.size
    val priorDemandModel = PriorLogDemandModel(trainProductItems, avgLogWeeklySaleDAO, null)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val segmentsByItem = SegmentsByItem(trainProductItems)

    logger.info("Building gp models...")
    val gpModelsBySegment = trainProductItems.groupBy { item => segmentsByItem.getSegment2(item).get }.par.map {
      case (segmentId, depotItems) =>
        val gpModel = createGprModel(depotItems, meanLogDemand, priorDemandModel)
        (segmentId) -> gpModel
    }
    logger.info("Building gp models...done")
    logger.info("Predicting log demand..., size=" + testProductItems.size)

    val predictedProductDemand = testProductItems.par.map { item =>

      //  logger.info("Predicting item:" + item)
      if (trainSize == 0) (item, exp(priorDemandModel.predictLogDemand(item)) - 1)
      else {

        val segmentId2 = segmentsByItem.getSegment2(item)
        val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(5.54149)
        val x = extractFeatureVec(item, clientLogSale).toDenseMatrix

        val logDemand2 = segmentId2 match {
          case Some(segmentId) => {
            val gpModel = gpModelsBySegment(segmentId)

            val logDemand = try {
              gpModel.predictMean(x)(0)
            } catch {
              case e: Exception => {
                println(item)
                println(x)
                throw e
              }
            }
            logDemand
          }
          case None => {
            val sortedLogDemands = segmentsByItem.getSegmentIds().par.map { segmentId =>
              val gpModel = gpModelsBySegment.get(segmentId).get
              val logDemand = gpModel.predict(x)(0, ::)
              segmentId -> logDemand.t
            }.toList.sortBy(x => x._2(1))

            segmentsByItem.addItemSegment(item, sortedLogDemands.head._1)
            sortedLogDemands.head._2(0)
          }
        }

        val demand = exp(logDemand2) - 1

        (item, demand)
      }
    }.toList
    logger.info("Predicting log demand...done")
    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], meanLogDemand: Double, priorDemandModel: PriorLogDemandModel): GprPredictEngine = {
    val x = extractFeatureVec(items, avgLogWeeklySaleDAO)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val covFunc = SegmentProductCovFunc()
    val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1))
    val noiseLogStdDev = log(1)

    //    val covFuncParams = DenseVector(0.8747988932392572, 1.1583621879204218, -1.0580826228536655, -0.31826092507377174, -0.29788560362041744, -1.2138633684719795)
    //   val noiseLogStdDev = -1.0363286325004006

    val meanVec = DenseVector.zeros[Double](x.rows) + meanLogDemand

    def meanFunc(x: DenseMatrix[Double]): DenseVector[Double] = {
      val meanVec = x(*, ::).map { x =>
        val clientId = x(1).toInt
        //  priorDemandModel.predictLogDemand(clientId)
        meanLogDemand
      }
      meanVec
    }

    var modelEngine: GprPredictEngine = null

    while (modelEngine == null) {
      try {

        val model = new GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, meanFunc)
        modelEngine = GprPredictEngine(model)
      } catch {
        case e: Exception => {
          logger.error(e.getLocalizedMessage + ":" + items.size + ":" + items.head)
        }
      }
    }
    modelEngine
  }
}