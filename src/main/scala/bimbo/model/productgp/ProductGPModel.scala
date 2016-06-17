package bimbo.model.productgp

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.dao.AvgLogWeeklySaleDAO
import dk.gp.gpr.GprModel
import breeze.numerics._
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import bimbo.model.clientproductgp.priordemand.createSalesDemandData
import dk.gp.gpr.gpr
import dk.gp.util.loadObject
import bimbo.data.dao.ItemSegmentDAO
import bimbo.data.dao.SegmentGPParamsDAO
import bimbo.data.dao.ItemByProductDAO

case class ProductGPModel(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO,
                          productGPModelParamsFile: String, itemSegmentDAO: ItemSegmentDAO, segmentGPParamsDAO: SegmentGPParamsDAO) extends DemandModel {

  private val modelParamsByProductId = loadObject[Map[Int, (Array[Double], Double)]](productGPModelParamsFile)

  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)
    val trainProductItemsBySegment: Map[Int, Seq[(Int, Item)]] = trainProductItems.map(item => (itemSegmentDAO.getSegment(item), item)).groupBy(_._1)

    val testProductItemsBySegment: Map[Int, Seq[(Int, Item)]] = testProductItems.map(item => (itemSegmentDAO.getSegment(item), item)).groupBy(_._1)

    val predictedDemand = testProductItemsBySegment.toList.flatMap {
      case (segmentId, testSegmentItems) =>

        val trainSegmentItems = trainProductItemsBySegment(segmentId).map(i => i._2)
        predictSegmentDemand(segmentId, trainSegmentItems, testSegmentItems.map(i => i._2))
    }
    predictedDemand
  }

  private def predictSegmentDemand(segmentId: Int, trainSegmentItems: Seq[Item], testSegmentItems: Seq[Item]): Seq[(Item, Double)] = {

    val x = extractFeatureVec(trainSegmentItems, avgLogWeeklySaleDAO)
    val y = DenseVector(trainSegmentItems.map(i => log(i.demand + 1)).toArray)

    val (covFuncParams, noiseLogStdDev) = segmentGPParamsDAO.getSegmentGPParams(segmentId)

    val gprModel = GprModel(x, y, ProductCovFunc(), covFuncParams, noiseLogStdDev)

    val predictedDemand = testSegmentItems.map { item =>

      val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).get //.getOrElse(5.54149)
      val x = extractFeatureVec(item, clientLogSale).toDenseMatrix
      val logDemand = dk.gp.gpr.predict(x, gprModel)(0, 0)

      val demand = exp(logDemand) - 1
      item -> demand
    }
    predictedDemand
  }

  def predictProductDemand2(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val x = extractFeatureVec(trainProductItems, avgLogWeeklySaleDAO)
    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)

    val (covFuncParams, noiseLogStdDev) = modelParamsByProductId(productId)
    val gprModel = GprModel(x, y, ProductCovFunc(), DenseVector(covFuncParams), noiseLogStdDev)

    val predicted = productItems.map { item =>

      val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(5.54149)
      val x = extractFeatureVec(item, clientLogSale).toDenseMatrix
      val logDemand = dk.gp.gpr.predict(x, gprModel)(0, 0)

      val demand = exp(logDemand) - 1
      item -> demand
    }

    predicted
  }
}