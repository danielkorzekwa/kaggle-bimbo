package bimbo

import bimbo.data.dao.ItemDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.model.clientproductgp.priordemand.createSalesDemandData
import dk.gp.gpr.GprModel
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import breeze.numerics._
import dk.gp.gpr.gpr
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.gpr.predict
import breeze.linalg.DenseMatrix
import dk.gp.mtgpr.mtgprTrain
import dk.gp.mtgpr.MtGprModel
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.model.clientproductgp.RouteCovFunc

object TrainClientProductPriorApp extends LazyLogging {

   val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
    val itemDAO = ItemDAO(allItemsDAO)

  val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  def main(args: Array[String]): Unit = {

    trainGrp()
  }

  def trainMtgpr() = {

    // val productIds = itemDAO.getProductIds()
    val productIds = List(1240)
    val trainingData = productIds.map { productId =>
      val productItems = itemDAO.getProductItems(productId)
      val (x, y) = createSalesDemandData(productItems, avgLogWeeklySaleByClientDAO)
      DenseMatrix.horzcat(x, y.toDenseMatrix.t)
    }
    logger.info("Data size:" + trainingData.size)
    val mtgprModel = MtGprModel(trainingData, CovSEiso(), DenseVector(log(1), log(1)), log(1))
    val trainedModel = mtgprTrain(mtgprModel)
    println("covFuncParams=%s, noiseLogStdDev=%f".format(trainedModel.covFuncParams, trainedModel.likNoiseLogStdDev))

  }

  def trainGrp() = {
    val items = itemDAO.getProductItems(1240)
    val (x, y) = createSalesDemandData(items, avgLogWeeklySaleByClientDAO)

    logger.info("Data size:" + x.rows)

    val gprModel = gpr(x, y, RouteCovFunc(), DenseVector(log(1), log(1),log(1), log(1)), log(1))
    //  val gprModel = GprModel(x, y, CovSEiso(), DenseVector(log(1), log(1)), log(1))
    println("covFuncParams=%s, noiseLogStdDev=%f".format(gprModel.covFuncParams, gprModel.noiseLogStdDev))

    val xTest = DenseVector.rangeD(0, 17, 1).toDenseMatrix.t
    val predicted = predict(xTest, gprModel)(::, 0)

    println(DenseMatrix.horzcat(xTest, predicted.toDenseMatrix.t))
  }
}