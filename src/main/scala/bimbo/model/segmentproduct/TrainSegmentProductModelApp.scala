package bimbo.model.segmentproduct

import dk.gp.mtgpr.MtGprModel
import breeze.linalg.DenseVector
import breeze.numerics._
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import breeze.stats._
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.linalg.DenseMatrix
import dk.gp.mtgpr.mtgprTrain

object TrainSegmentProductModelApp {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo//train_3_to_8.csv", clientNamesDAO)
  val trainItemDAO = ItemByProductDAO(allItemsDAO)
  val avgLogWeeklySaleDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  def main(args: Array[String]): Unit = {

    val trainProductItems = trainItemDAO.getProductItems(43231)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val segmentsByItem = SegmentsByItem(trainProductItems)

    val data = trainProductItems.groupBy { item => segmentsByItem.getSegment2(item).get }.map {
      case (segmentId, segmentItems) =>
        val x = extractFeatureVec(segmentItems, avgLogWeeklySaleDAO)
        val y = DenseVector(segmentItems.map(i => log(i.demand + 1)).toArray) - meanLogDemand

        DenseMatrix.horzcat(x, y.toDenseMatrix.t)

    }.toList.take(10)

    val covFunc = SegmentProductCovFunc()
    val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1))
    val noiseLogStdDev = log(1)

    val mtgprModel = MtGprModel(data, covFunc, covFuncParams, noiseLogStdDev)
    
     val trainedModel = mtgprTrain(mtgprModel)
    println("covFuncParams=%s, noiseLogStdDev=%f".format(trainedModel.covFuncParams, trainedModel.likNoiseLogStdDev))

  }
}