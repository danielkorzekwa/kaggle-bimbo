package bimbo.model.clientproducthgpr

import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.Item
import bimbo.model.DemandModel
import breeze.linalg.DenseVector
import breeze.numerics._
import dk.gp.cov.CovSEiso
import dk.gp.hgpr.HgprModel
import breeze.linalg.DenseMatrix
import dk.gp.hgpr.hgprPredict
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.stats._

case class ClientProductHgprModel(trainItemDAO: ItemByProductDAO,avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO) extends DemandModel with LazyLogging {

  def predictProductDemand(productId: Int, testItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)//.take(1000)

    val productItemsByClient = trainProductItems.groupBy { i => i.clientId }
//    logger.info("Preparing training data...")
//    val xData = productItemsByClient.map {
//      case (clientId, clientItems) =>
//
//        val x = extractFeatureVec(clientItems)
//        val y = DenseVector(clientItems.map(i => log(i.demand + 1)).toArray)
//        DenseMatrix.horzcat(x, y.toDenseMatrix.t)
//    }.toList
//    logger.info("Preparing training data...done: " + xData.size)

    val x = extractFeatureVec(trainProductItems,avgLogWeeklySaleByClientDAO,0)
    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)

    val yMean = mean(y)
    val covFunc = ClientProductHgprCovFunc()
    val covFuncParams = DenseVector(log(1), log(1), log(1),log(1))
    val noiseLogStdDev = log(1)
    
    
   
    //  val covFuncParams = DenseVector(-0.9268993114646398, 2.445784282036537, 0.141445422730983)
  //  val noiseLogStdDev =  -0.8721711213924357
    
  

    val u = calcU(x)

    val hgprModel = HgprModel(x, y-yMean, u, covFunc, covFuncParams, noiseLogStdDev)

    val defaultClientLogSale = mean(x(::,0))
    val xTest = extractFeatureVec(testItems,avgLogWeeklySaleByClientDAO,defaultClientLogSale)

   val logDemand =  hgprPredict(xTest, hgprModel).map(x => x.m+yMean)
   
   val predictedDemand = (0 until testItems.size).map{i =>
     val item = testItems(i)
     val itemLogDemand = logDemand(i)
     val demand = exp(itemLogDemand)-1
     
     (item,demand)
   }
   predictedDemand
  }
}