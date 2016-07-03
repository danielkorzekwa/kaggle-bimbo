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
import bimbo.model.segmentproduct.util.calcNewProductMap
import bimbo.model.segmentproduct.util.FeatureVectorFactory

case class SegmentProductModel(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel with LazyLogging {

  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    logger.info("Predicting for product: " + productId)

    val trainProductItems = trainItemDAO.getProductItems(productId)
    val trainSize = trainProductItems.size

    val trainAndTestItems = trainProductItems ++ testProductItems
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainAndTestItems)
    val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap)

    val priorDemandModel = PriorLogDemandModel(trainProductItems, avgLogWeeklySaleDAO, null)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val segmentsByItem = SegmentsByItem(trainProductItems)

    logger.info("Building gp models...")
    val gpModelsBySegment = trainProductItems.groupBy { item => segmentsByItem.getSegment(item).get }.par.map {
      case (segmentId, depotItems) =>
        val gpModel = createGprModel(depotItems, meanLogDemand, priorDemandModel, featureVectorFactory)
        (segmentId) -> gpModel
    }
    logger.info("Building gp models...done")

    logger.info("Predicting log demand..., size=" + testProductItems.size)
    val predictedProductDemand = testProductItems.par.map { item =>

      //  logger.info("Predicting item:" + item)
      if (trainSize == 0) (item, exp(priorDemandModel.predictLogDemand(item)) - 1)
      else {
  
        val segmentId = segmentsByItem.getSegment(item)
        val x = featureVectorFactory.create(item).toDenseMatrix
        val logDemand2 = segmentId match {
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
          case _ => {
                    
            val sortedLogDemands = segmentsByItem.getSegmentIds().par.map { segmentId =>
              val gpModel = gpModelsBySegment.get(segmentId).get
              val logDemand = gpModel.predict(x)(0, ::)
              segmentId -> logDemand.t
            }.toList.sortBy(x => x._2(1))
     
         segmentsByItem.addItemSegment(item, sortedLogDemands.head._1)
            
           val logDemand = sortedLogDemands.head._2(0) 
            logDemand
          }
        }

        val demand = exp(logDemand2) - 1

        (item, demand)
      }
    }.toList
    logger.info("Predicting log demand...done")
    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], meanLogDemand: Double, priorDemandModel: PriorLogDemandModel,
                             featureVectorFactory: FeatureVectorFactory): GprPredictEngine = {
    val x = featureVectorFactory.create(items)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val covFunc = SegmentProductCovFunc2()
    val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1),log(1))
    val noiseLogStdDev = log(1)

     //   val covFuncParams = DenseVector(0.4825566762220455, 0.8804206240310104, -1.0475481756870566, -1.538927576519421, -1.4900115584427644, -1.487285360420513, -2.2779017293273958)
     //  val noiseLogStdDev = -1.060649


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