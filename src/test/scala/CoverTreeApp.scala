import smile.neighbor.LinearSearch
import smile.math.distance.EuclideanDistance
import smile.math.distance.Metric
import smile.math.distance.Distance
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import smile.neighbor.CoverTree
import breeze.linalg.DenseVector
import bimbo.model.knngp2.KnnGP2CovFunc
import breeze.numerics._
import bimbo.data.Item
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.model.segmentproduct.util.calcNewProductMap
import bimbo.model.knngp2.util.FeatureVectorFactory
import bimbo.model.knngp2.knn.BruteKnn
import com.typesafe.scalalogging.slf4j.LazyLogging
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseMatrix

object CoverTreeApp extends LazyLogging {

  val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
  val testItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
  val testItemByProductDAO = ItemByProductDAO(testItemsDAO)
  val avgLogWeeklySaleDAO = AvgLogWeeklySaleDAO("/mnt/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

  val testItems = testItemByProductDAO.getProductItems(1240).toArray

  val covFuncParams = DenseVector(log(1), log(1), log(1), log(1), log(1), log(1), log(1))
  val noiseLogStdDev = log(1)
  val covFunc = KnnGP2CovFunc()

  val newProductMap: Map[Item, Boolean] = calcNewProductMap(testItems)
  val featureVectorFactory = FeatureVectorFactory(avgLogWeeklySaleDAO, newProductMap)

  def main(args: Array[String]): Unit = {

    

    val data = (1 to 1).flatMap(i => testItems).map {item =>
      featureVectorFactory.create(item).toArray
    }.toArray.take(50000)
logger.info("train data:" + data.size)
    
    logger.info("Building knn model...")
   //  val knnModel = BruteKnn(data, covFunc, covFuncParams, featureVectorFactory)

    //  val model = new LinearSearch(data, ItemDistance(covFunc,covFuncParams))
    val model = new CoverTree(data, ItemDistance(covFunc,covFuncParams))
    //model.setIdenticalExcluded(false)

    logger.info("Predicting...")
    val now = System.currentTimeMillis()
    data.take(1000).par.foreach { item =>
      model.knn(item, 100)
    //   knnModel.getKNN(item, 100)
    }

    println(System.currentTimeMillis() - now)
  }

  
}