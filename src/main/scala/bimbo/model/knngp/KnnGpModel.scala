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
import dk.gp.gpr.GprPredictEngine
import bimbo.model.segmentproduct.util.calcNewProductMap
import bimbo.model.knngp.util.FeatureVectorFactory
import bimbo.model.knngp.util.FeatureVectorFactory
import bimbo.model.clientproductgp.priordemand.PriorLogDemandModel

case class KnnGpModel(trainItemDAO: ItemByProductDAO, avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends DemandModel {

   // val covFuncParams = DenseVector(0.5100862632786983, 1.0923800425633166, -0.7729930012256185, -1.3025016809868923, -1.3214099799271923, -1.399525821347226, -2.100525043165405)
  //  val noiseLogStdDev = log(1)
  
 //  val covFuncParams = DenseVector(0.5100862632786983, 1.0923800425633166, -0.7729930012256185, -1.3025016809868923, -1.3214099799271923, -1.399525821347226, -2.100525043165405)
  //  val noiseLogStdDev = -0.963068
    
  //  val covFuncParams = DenseVector(0.36952393709334364, 0.6792218479309723, -1.067240272041124, -0.5003187242401969, -0.4527152017680887, -1.6057270907193402, -2.0658541053386887)
  //  val noiseLogStdDev = -1.056311
  
   val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1))
      val noiseLogStdDev = log(1)
  
  def predictProductDemand(productId: Int, testProductItems: Seq[Item]): Seq[(Item, Double)] = {

    val trainProductItems = trainItemDAO.getProductItems(productId)

     val priorDemandModel = PriorLogDemandModel(trainProductItems, avgLogWeeklySaleDAO, null)

    
    val trainAndTestItems = trainProductItems ++ testProductItems
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainAndTestItems)
    val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap)

    val y = DenseVector(trainProductItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

     val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1))
    // val covFuncParams = DenseVector(0.36952393709334364, 0.6792218479309723, -1.067240272041124, -0.5003187242401969, -0.4527152017680887, -1.6057270907193402, -2.0658541053386887)
   
    val itemClusterBuilder = ItemClusterBuilder(KnnGPCovFunc(), covFuncParams, threshold = 4.5, featureVectorFactory)

    trainProductItems.foreach { item =>
      itemClusterBuilder.processItem(item)
    }
    println("num of clusters=" + itemClusterBuilder.getClusters().size)
    itemClusterBuilder.getClusters().toList.sortBy(_._2.size).foreach { case (item, items) => println(items.size) }

    logger.info("Building gp models...")
    val gpModelsBySegment = itemClusterBuilder.getClusters().par.map {
      case (item, items) =>

        val nearestItems = itemClusterBuilder.getNNearestItems(item, 100).take(100)

        println(nearestItems.size)
        val gpModel = createGprModel(nearestItems, meanLogDemand, featureVectorFactory,priorDemandModel)
        (item) -> gpModel
    }
    logger.info("Building gp models...done")

    val predictedProductDemand = testProductItems.par.map { item =>

      val (cluster, covVal) = itemClusterBuilder.getNearestCluster(item).head

      val x = featureVectorFactory.create(item).toDenseMatrix

      val gpModel = gpModelsBySegment(cluster)

      val logDemand = gpModel.predictMean(x)(0)

      val demand = exp(logDemand) - 1
      (item, demand)
    }.toList

    predictedProductDemand
  }

  private def createGprModel(items: Seq[Item], meanLogDemand: Double, featureVectorFactory: FeatureVectorFactory,
      priorDemandModel:PriorLogDemandModel): GprPredictEngine = {
    val x = featureVectorFactory.create(items)
    val y = DenseVector(items.map(i => log(i.demand + 1)).toArray)

    val covFunc = KnnGPCovFunc()
  

    val meanVec = DenseVector.zeros[Double](x.rows) + meanLogDemand

    def meanFunc(x: DenseMatrix[Double]): DenseVector[Double] = {
      val meanVec = x(*, ::).map { x =>
        val clientId = x(1).toInt
          priorDemandModel.predictLogDemand(clientId)
        //meanLogDemand
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
    GprPredictEngine(model)
  }
}