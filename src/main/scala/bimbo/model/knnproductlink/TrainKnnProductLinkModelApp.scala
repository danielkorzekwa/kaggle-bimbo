package bimbo.model.knnproductlink

import dk.gp.mtgpr.mtgprTrain
import bimbo.model.knngp2.KnnGP2CovFunc2
import breeze.linalg.DenseVector
import breeze.numerics._
import dk.gp.mtgpr.MtGprModel
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.data.dao.ProductDAO
import bimbo.data.PgProductDetails
import bimbo.model.knngp2.knn.CoverTreeKnn
import bimbo.model.knngp2.util.FeatureVectorFactory
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.model.knngp2.util.calcNewProductMap
import bimbo.data.Item
import breeze.stats._
import breeze.linalg.DenseMatrix
import scala.util.Random
import bimbo.data.dao.clientname.ClientNameDAO
import bimbo.data.dao.townstate.TownStateDAO
import bimbo.model.knngp2.KnnARDCovFunc

object TrainKnnProductLinkModelApp {

  val clientNamesDAO = ClientNamesDAO("/mnt/bimbo/cliente_tabla.csv")
  val allItemsDAO = AllTrainItemsDAO("/mnt/bimbo//train_3_to_8.csv", clientNamesDAO)

  val productMap = ProductDAO("/mnt/bimbo/producto_tabla.csv").getProductMap()
  val trainItemByPgProductDAO = ItemByPgProductDAO(allItemsDAO, productMap)

  val trainItems = trainItemByPgProductDAO.getProductItems(PgProductDetails("Canelitas", 1, 120))

  val avgLogWeeklySaleDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  val townStateMap = TownStateDAO("/mnt/bimbo/town_state.csv").getTownStateMap()
val clientNameIdMap = ClientNameDAO("/mnt/bimbo/cliente_tabla.csv").getClientNameIdMap()

  val newProductMap: Map[Item, Boolean] = calcNewProductMap(trainItems)
  val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap, townStateMap,clientNameIdMap)

  val covFunc = KnnARDCovFunc()
  //val covFuncParams = DenseVector(log(1), log(1))
  val covFuncParams = DenseVector(log(1), log(1),log(1), log(1),log(1), log(1))
  val noiseLogStdDev = log(1)

  def main(args: Array[String]): Unit = {

    val y = DenseVector(trainItems.map(i => log(i.demand + 1)).toArray)
    val meanLogDemand = mean(y)

    val knnModel = CoverTreeKnn(trainItems.toArray,  featureVectorFactory)

    val data = new Random(3450).shuffle(trainItems).take(1000).map { item =>

      val trainItems = knnModel.getKNN(item, 100)
      val xKnn = DenseVector.horzcat(trainItems.map(_.x): _*).t
      val yKnn = DenseVector(trainItems.map(point => log(point.demand + 1)).toArray) - meanLogDemand
      DenseMatrix.horzcat(xKnn, yKnn.toDenseMatrix.t)
    }

    val mtgprModel = MtGprModel(data, covFunc, covFuncParams, noiseLogStdDev)

    val trainedModel = mtgprTrain(mtgprModel, maxIter = 20)
    println("covFuncParams=%s, noiseLogStdDev=%f".format(trainedModel.covFuncParams, trainedModel.likNoiseLogStdDev))
  }
}