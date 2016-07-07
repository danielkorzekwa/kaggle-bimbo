package bimbo.model.knnproductlink

import bimbo.model.DemandModel
import bimbo.data.Item
import bimbo.data.PgProductDetails
import bimbo.data.ProductDetails
import breeze.linalg.DenseVector
import bimbo.data.GenericProductDetails
import java.util.concurrent.atomic.AtomicInteger
import bimbo.model.clientproductgp.ClientProductGPModel
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.linkedmodel.client.ClientModel2
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.model.knngp2.knnGpPredict
import bimbo.data.dao.townstate.TownState

case class KnnProductLinkModel(productMap: Map[Int, ProductDetails], trainItemDAO: ItemByProductDAO, avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO,
                               trainItemByPgProductDAO: ItemByPgProductDAO, townStateMap: Map[Int, TownState],clientNameMap:Map[Int,Int]  ) extends LazyLogging {

  def predict(testItems: Seq[Item]): DenseVector[Double] = {

    val pgTestItems = testItems.filter(item => productMap(item.productId).isInstanceOf[PgProductDetails])
    val genericTestItems = testItems.filter(item => productMap(item.productId).isInstanceOf[GenericProductDetails])

    val predictddDemandGeneric = predictGeneric(genericTestItems)
    val predictedDemandGP = predictPG(pgTestItems)

    val predictedDemandByItem: Map[Item, Double] = (predictedDemandGP ++ predictddDemandGeneric).toMap
    val predictedDemand = DenseVector(testItems.map(i => predictedDemandByItem(i)).toArray)
    predictedDemand
  }

  def predictPG(testItems: Seq[Item]): Seq[(Item, Double)] = {
    val testItemsByProduct = testItems.groupBy { item => productMap(item.productId) }

    val i = new AtomicInteger(1)
    val predictedDemand = testItemsByProduct.toList.flatMap {
      case (productDetails: PgProductDetails, testItems) =>

        val trainItems = trainItemByPgProductDAO.getProductItems(productDetails)
        if (i.getAndIncrement % 1 == 0) logger.info(
          "Predicting pgProduct %d/%d, trainSize/testSize=%d/%d, product=%s".format(i.get, testItemsByProduct.size, trainItems.size, testItems.size, productDetails))

        knnGpPredict(trainItems.toArray, testItems, avgLogWeeklySaleByClientDAO, townStateMap,clientNameMap)

    }

    predictedDemand
  }

  def predictGeneric(testItems: Seq[Item]): Seq[(Item, Double)] = {

    val testItemsByProduct = testItems.groupBy { i => i.productId }

    val i = new AtomicInteger(1)
    val predictedDemand = testItemsByProduct.toList.flatMap {
      case (productId, productItems) =>
        if (i.getAndIncrement % 10 == 0) logger.info("Predicting product %d/%d".format(i.get, testItemsByProduct.size))
        //ClientProductGPModel(trainItemDAO, avgLogWeeklySaleByClientDAO,null).predictProductDemand(productId, productItems)

        val trainProductItems = trainItemDAO.getProductItems(productId).toArray
        knnGpPredict(trainProductItems, productItems, avgLogWeeklySaleByClientDAO, townStateMap,clientNameMap)
    }

    predictedDemand
  }
}