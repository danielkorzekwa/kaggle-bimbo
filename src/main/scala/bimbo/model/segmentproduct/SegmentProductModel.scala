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
import dk.gp.gpr.gprPredictMean
import java.util.concurrent.atomic.AtomicInteger

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
              gprPredictMean(x, gpModel)(0)
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
              val logDemand = gprPredict(x, gpModel)(0, ::)
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

  private def createGprModel(items: Seq[Item], meanLogDemand: Double, priorDemandModel: PriorLogDemandModel): GprModel = {
    val x = extractFeatureVec(items, avgLogWeeklySaleDAO)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val covFunc = SegmentProductCovFunc()
   val covFuncParams = DenseVector(log(1), log(1), log(1), log(1),log(1),log(1))
    val noiseLogStdDev = log(1)
    
   //  val covFuncParams = DenseVector(0.8079692974725987, 0.8620529637197376, -1.2749748663379514, -1.7515573778429132, -3.355482028065813, -2.7077662216789244)
   // val noiseLogStdDev = -1.070261

    val meanVec = DenseVector.zeros[Double](x.rows) + meanLogDemand

    def meanFunc(x: DenseMatrix[Double]): DenseVector[Double] = {
      val meanVec = x(*, ::).map { x =>
        val clientId = x(1).toInt
        //  priorDemandModel.predictLogDemand(clientId)
        meanLogDemand
      }
      meanVec
    }

    var model: GprModel = null

    while (model == null) {
      try {

        model = new GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, meanFunc)

      } catch {
        case e: Exception => {
          logger.error(e.getLocalizedMessage + ":" + items.size + ":" + items.head)
        }
      }
    }
    model
  }
}