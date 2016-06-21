package bimbo.linkedmodel

import breeze.linalg.DenseVector
import bimbo.data.Item
import bimbo.data.dao.ProductDAO
import bimbo.data.ProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.GenericProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.GenericProductDetails
import bimbo.data.GenericProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.model2.client.ClientModel
import bimbo.linkedmodel.client.ClientModel2
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import bimbo.data.dao.allitems.AllItemsDAO
import bimbo.model.clientproductgp.ClientProductGPModel
import bimbo.data.dao.ItemByProductDAO

abstract class LinkedDemandModel(productMap: Map[Int, ProductDetails], trainItemByPgProductDAO: ItemByPgProductDAO, avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO,
    trainItemDAO: ItemByProductDAO)
    extends LazyLogging {

  def predict(testItems: Seq[Item]): DenseVector[Double] = {

    val pgTestItems = testItems.filter(item => productMap(item.productId).isInstanceOf[PgProductDetails])
    val genericTestItems = testItems.filter(item => productMap(item.productId).isInstanceOf[GenericProductDetails])

    val predictedDemandGP = predictPG(pgTestItems)
    val predictddDemandGeneric = predictGeneric(genericTestItems)

    val predictedDemandByItem: Map[Item, Double] = (predictedDemandGP ++ predictddDemandGeneric).toMap
    val predictedDemand = DenseVector(testItems.map(i => predictedDemandByItem(i)).toArray)
    predictedDemand
  }

  def predictPG(testItems: Seq[Item]): Seq[(Item, Double)] = {

    val testItemsByProduct = testItems.groupBy { item => productMap(item.productId) }

    val i = new AtomicInteger(1)
    val predictedDemand = testItemsByProduct.toList.par.flatMap {
      case (productDetails: PgProductDetails, testItems) =>
       
        val trainItems = trainItemByPgProductDAO.getProductItems(productDetails)
         if (i.getAndIncrement % 1 == 0) logger.info(
             "Predicting pgProduct %d/%d, trainSize/testSize=%d/%d, product=%s".format(i.get, testItemsByProduct.size, trainItems.size,testItems.size,productDetails))
     
        val clientModel = ClientModel2(avgLogWeeklySaleByClientDAO)
        clientModel.predictDemand(trainItems, testItems)

    }.toList

    predictedDemand
  }

  def predictGeneric(testItems: Seq[Item]): Seq[(Item, Double)] = {

    val testItemsByProduct = testItems.groupBy { i => i.productId }

    val i = new AtomicInteger(1)
    val predictedDemand = testItemsByProduct.toList.par.flatMap {
      case (productId, productItems) =>
        if (i.getAndIncrement % 10 == 0) logger.info("Predicting product %d/%d".format(i.get, testItemsByProduct.size))
        ClientProductGPModel(trainItemDAO, avgLogWeeklySaleByClientDAO)predictProductDemand(productId, productItems)
    }.toList

    predictedDemand
  }

  def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] 
}