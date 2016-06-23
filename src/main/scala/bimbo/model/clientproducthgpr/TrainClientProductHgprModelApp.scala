package bimbo.model.clientproducthgpr

import dk.gp.hgpr.hgprTrain
import dk.gp.hgpr.HgprModel
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import breeze.numerics._
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.stats._

object TrainClientProductHgprModelApp {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_8.csv", clientNamesDAO)
  val trainItemDAO = ItemByProductDAO(allItemsDAO)

  val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_8.csv")

  def main(args: Array[String]): Unit = {

    val trainProductItems = trainItemDAO.getProductItems(43186)//.take(1000)

    val x = extractFeatureVec(trainProductItems, avgLogWeeklySaleByClientDAO, 0)
    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)

    val yMean = mean(y)

    val covFunc = ClientProductHgprCovFunc()
    val covFuncParams = DenseVector(log(1), log(1), log(1))
    val noiseLogStdDev = log(1)

    val u = calcU(x)

    val hgprModel = HgprModel(x, y - yMean, u, covFunc, covFuncParams, noiseLogStdDev)

    hgprTrain(hgprModel)
  }
}