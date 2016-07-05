import smile.neighbor.LinearSearch
import smile.math.distance.EuclideanDistance
import smile.math.distance.Metric
import smile.math.distance.Distance
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import smile.neighbor.CoverTree
import breeze.linalg.DenseVector
import breeze.numerics._
import bimbo.data.Item
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.model.segmentproduct.util.calcNewProductMap
import bimbo.model.knngp2.util.FeatureVectorFactory
import com.typesafe.scalalogging.slf4j.LazyLogging
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseMatrix
import bimbo.model.knngp2.KnnGP2CovFunc2
import bimbo.model.knngp2.knn.ItemDistance
import bimbo.model.knngp2.knn.LinearKnn
import bimbo.model.knngp2.knn.CoverTreeKnn

object CoverTreeApp extends LazyLogging {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
  val testItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
  val testItemByProductDAO = ItemByProductDAO(testItemsDAO)
  val avgLogWeeklySaleDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  val testItems = testItemByProductDAO.getProductItems(1240).toArray.take(400000)

  val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1))
  val noiseLogStdDev = log(1)
  val covFunc = KnnGP2CovFunc2()

  val newProductMap: Map[Item, Boolean] = calcNewProductMap(testItems)
  val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap)

  def main(args: Array[String]): Unit = {

    logger.info("train data:" + testItems.size)

    logger.info("Building knn model...")
    //  val model = BruteKnn2(testItems, covFunc, covFuncParams, featureVectorFactory)
  //  val model = LinearKnn(testItems, covFunc, covFuncParams, featureVectorFactory)
    val model = new CoverTreeKnn(testItems, covFunc, covFuncParams, featureVectorFactory)
    //model.setIdenticalExcluded(false)

    logger.info("Predicting...")
    val now = System.currentTimeMillis()
    testItems.take(10000).par.foreach { item =>
      val knnItems = model.getKNN(item, 100)
      //println(knnItems.size)
      //println("item:" + featureVectorFactory.create(item))
      //knnItems.take(10).foreach(println(_))
     // println(featureVectorFactory.create(item).toString + ":" + knnItems)
    }

    println(System.currentTimeMillis() - now)
  }

}