package bimbo.model.clientproductgp

import bimbo.model.DemandModel
import breeze.linalg.DenseVector
import bimbo.data.Item
import dk.gp.gpr.GprModel
import dk.gp.gpr.GprModel
import breeze.linalg.DenseMatrix
import breeze.numerics._
import dk.gp.cov.CovSEiso
import dk.gp.gpr.GprModel
import dk.gp.gpr.GprModel
import java.util.concurrent.atomic.AtomicInteger
import org.apache.spark.util.StatCounter
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import bimbo.data.dao.AvgLogWeeklySaleDAO
import dk.gp.cov.CovSEiso
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.AvgLogDemandByClientDAO
import dk.gp.gpr.gprPredict
import dk.gp.gpr.GprPredictEngine

case class ClientProductGPModel(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO,
    avgLogDemandDAO:AvgLogDemandByClientDAO)
    extends DemandModel with LazyLogging {

  def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val priorDemandModel = PriorLogDemandModel(trainProductItems, avgLogWeeklySaleByClientDAO,avgLogDemandDAO)

    val gpModelsByClientProduct = trainProductItems.groupBy { i => getKey(i) }.map {
      case ((clientId, productId), clientProductItems) =>
        val priorLogDemand = priorDemandModel.predictLogDemand(clientProductItems.head)
        val gpModel = createGprModel(clientProductItems, priorLogDemand)
        (clientId, productId) -> gpModel
    }
    val predictedProductDemand = productItems.map { item =>

     
      val gpModel = gpModelsByClientProduct.get(getKey(item))
      val logDemand = gpModel match {
        case Some(gpModel)  => {
          
          val x = extractFeatureVec(item).toDenseMatrix
          val logDemand = gpModel.predictMean(x)(0)
         
          logDemand
        }
        case _ => priorDemandModel.predictLogDemand(item)
      }

      val demand = exp(logDemand) - 1
 
      (item, demand)
    }

    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], demandMean: Double): GprPredictEngine = {
    val x = extractFeatureVec(items)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

   
    

    
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)

    //train 
//        val covFunc = CovSEiso()
//    val covFuncParams = DenseVector(-1.5659136180331088, 0.0)
//    val noiseLogStdDev = -0.619037
    GprPredictEngine(GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, mean = demandMean))
  }

  private def getKey(item: Item): (Int, Int) = (item.clientId, item.productId)
}

