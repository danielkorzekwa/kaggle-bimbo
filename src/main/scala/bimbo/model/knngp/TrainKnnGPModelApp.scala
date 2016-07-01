package bimbo.model.knngp

import breeze.linalg.DenseMatrix
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.dao.ItemByProductDAO
import breeze.linalg.DenseVector
import dk.gp.mtgpr.MtGprModel
import dk.gp.mtgpr.mtgprTrain
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.model.segmentproduct.SegmentProductCovFunc
import breeze.linalg._
import breeze.numerics._
import bimbo.model.segmentproduct.util.calcNewProductMap
import bimbo.data.Item
import bimbo.model.knngp.util.FeatureVectorFactory
import breeze.stats._

object TrainKnnGPModelApp {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo//train_3_to_8.csv", clientNamesDAO)
  val trainItemDAO = ItemByProductDAO(allItemsDAO)
  val avgLogWeeklySaleDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  def main(args: Array[String]): Unit = {

    val trainProductItems = trainItemDAO.getProductItems(43231)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val covFunc = SegmentProductCovFunc()
    val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1))
    val noiseLogStdDev = log(1)

    val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainProductItems)
    val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap)

    val itemClusterBuilder = ItemClusterBuilder(KnnGPCovFunc(), covFuncParams, threshold = 3.5, featureVectorFactory)
    trainProductItems.foreach { item =>
      itemClusterBuilder.processItem(item)
    }
    println("num of clusters=" + itemClusterBuilder.getClusters().size)
    itemClusterBuilder.getClusters().toList.sortBy(_._2.size).foreach { case (item, items) => println(items.size) }

    val data = itemClusterBuilder.getClusters().map {
      case (item, items) =>

        val nearestItems = itemClusterBuilder.getNNearestItems(item, 200).take(200)

        val x = featureVectorFactory.create(nearestItems)
        val y = DenseVector(nearestItems.map(i => log(i.demand + 1)).toArray) - meanLogDemand
        DenseMatrix.horzcat(x, y.toDenseMatrix.t)
    }.toList

    val mtgprModel = MtGprModel(data, covFunc, covFuncParams, noiseLogStdDev)

    val trainedModel = mtgprTrain(mtgprModel)
    println("covFuncParams=%s, noiseLogStdDev=%f".format(trainedModel.covFuncParams, trainedModel.likNoiseLogStdDev))
  }
}