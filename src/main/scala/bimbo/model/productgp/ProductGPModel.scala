package bimbo.model.productgp

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.dao.ItemDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import dk.gp.gpr.GprModel
import breeze.numerics._
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import bimbo.model.clientproductgp.priordemand.createSalesDemandData
import dk.gp.gpr.gpr
import dk.gp.util.loadObject

case class ProductGPModel(trainItemDAO: ItemDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO,
    productGPModelParamsFile:String) extends DemandModel {

  private val modelParamsByProductId = loadObject[Map[Int, (Array[Double], Double)]](productGPModelParamsFile)
  
  def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)


    val x = extractFeatureVec(trainProductItems, avgLogWeeklySaleDAO)
    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)

    val (covFuncParams,noiseLogStdDev) = modelParamsByProductId(productId)
    val gprModel = GprModel(x, y, ProductCovFunc(), DenseVector(covFuncParams), noiseLogStdDev  )

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