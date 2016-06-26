package bimbo.model.knngp

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import dk.gp.math.sqDist
import breeze.linalg.DenseVector
import breeze.numerics._
import dk.gp.gpr.GprModel
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.stats._
import dk.gp.gpr.gprPredictMean
case class KnnGpModel(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel {

  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val covFuncParams = DenseVector(log(1), log(1), log(1), log(1),log(1), log(1))
    val itemClusterBuilder = ItemClusterBuilder(KnnGPCovFunc(), covFuncParams, threshold = 2.5, avgLogWeeklySaleDAO)

    trainProductItems.foreach { item =>
      itemClusterBuilder.processItem(item)
    }
    println("num of clusters=" + itemClusterBuilder.getClusters().size)
    itemClusterBuilder.getClusters().toList.sortBy(_._2.size).foreach { case (item, items) => println(items.size) }

    logger.info("Building gp models...")
    val gpModelsBySegment = itemClusterBuilder.getClusters().map {
      case (item, items) =>
        
        val nearestItems = itemClusterBuilder.getNNearestItems(item, 200).take(200)
        
        println(nearestItems.size)
        val gpModel = createGprModel(nearestItems, meanLogDemand)
        (item) -> gpModel
    }
  logger.info("Building gp models...done")
  
    val predictedProductDemand = testProductItems.map { item =>

      val (cluster, covVal) = itemClusterBuilder.getNearestCluster(item).head

      val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(5.54149)
      val x = extractFeatureVec(item, clientLogSale).toDenseMatrix

      val gpModel = gpModelsBySegment(cluster)

      val logDemand = gprPredictMean(x, gpModel)(0)

      val demand = exp(logDemand) - 1
      (item, demand)
    }

    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], meanLogDemand: Double): GprModel = {
    val x = extractFeatureVec(items, avgLogWeeklySaleDAO)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val covFunc = KnnGPCovFunc()
    val covFuncParams = DenseVector(log(1), log(1), log(1), log(1),log(1), log(1))
    val noiseLogStdDev = log(1)

    val meanVec = DenseVector.zeros[Double](x.rows) + meanLogDemand

    def meanFunc(x: DenseMatrix[Double]): DenseVector[Double] = {
      val meanVec = x(*, ::).map { x =>
        val clientId = x(1).toInt
        //  priorDemandModel.predictLogDemand(clientId)
        meanLogDemand
      }
      meanVec
    }

    var model: GprModel = null

    while (model == null) {
      try {

        model = new GprModel(x, y, covFunc, covFuncParams, noiseLogStdDev, meanFunc)

      } catch {
        case e: Exception => {
          logger.error(e.getLocalizedMessage + ":" + items.size + ":" + items.head)
        }
      }
    }
    model
  }
}