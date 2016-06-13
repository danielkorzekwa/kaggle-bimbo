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
        case Some(gpModel) => {
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

    //1240
    //    val covFunc = RouteCovFunc()
//     val covFuncParams = DenseVector(-1.14604754042564, 0.16010889770756165)
//     val noiseLogStdDev = -0.897265
    
    //1240
//     val covFunc = CovSEiso()
//    val covFuncParams = DenseVector(-1.1506291436326108, 0.0)
//    val noiseLogStdDev = -0.894726
    
    //2233
 //      val covFunc = CovSEiso()
 //   val covFuncParams = DenseVector(-1.0200623141474825, 0.0)
 //   val noiseLogStdDev = -1.002966
   
      //2233
//        val covFunc = RouteCovFunc()
//     val covFuncParams = DenseVector(-1.0160164727548524, 0.27396607977155757)
//     val noiseLogStdDev = -1.006402
    
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)
    GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, mean = demandMean)
  }

  private def getKey(item: Item): (Int, Int) = (item.clientId, item.productId)
}

