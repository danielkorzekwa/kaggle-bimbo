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
import bimbo.data.dao.ItemDAO
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel
import bimbo.data.dao.AvgLogWeeklySaleDAO
import dk.gp.cov.CovSEiso

case class ClientProductGPModel(trainItemDAO: ItemDAO, avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO)
    extends DemandModel with LazyLogging {

  def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val priorDemandModel = PriorLogDemandModel(trainProductItems, avgLogWeeklySaleByClientDAO)

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
          val logDemand = dk.gp.gpr.predict(x, gpModel)(0, 0)
          logDemand
        }
        case _ => priorDemandModel.predictLogDemand(item)
      }

      val demand = exp(logDemand) - 1

      (item, demand)
    }

    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], demandMean: Double): GprModel = {
    val x = extractFeatureVec(items)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

   
    
    //trained 1240 + filter client
//          val covFunc = RouteCovFunc()
//     val covFuncParams = DenseVector(-1.1603273944500738, 0.45693524709189554, 1.9678936712400013, 0.6542646482212601)
//     val noiseLogStdDev = -0.782495
    
    //trained 1240
//           val covFunc = RouteCovFunc()
//     val covFuncParams = DenseVector(-0.6987181874376821, -1.170222625034082E-4, -0.4449199487752958, -0.1924587820320028)
//     val noiseLogStdDev = -0.897733
     
      //trained 1250
//           val covFunc = RouteCovFunc()
//     val covFuncParams = DenseVector(-0.6165706186823657, 1.9499459350198003E-5, -0.37210796899448567, -0.17365292616006683)
//     val noiseLogStdDev = -0.879941
    
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)

    //train 31198
//        val covFunc = CovSEiso()
//    val covFuncParams = DenseVector(0.7826424318670169, 0.0)
//    val noiseLogStdDev = -0.413239
    GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, mean = demandMean)
  }

  private def getKey(item: Item): (Int, Int) = (item.clientId, item.productId)
}

