package bimbo.linkedmodel.client

import bimbo.linkedmodel.LinkedDemandModel
import bimbo.data.Item
import bimbo.data.ProductDetails
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.dao.ItemByProductDAO

case class ClientLinkedModel(productMap: Map[Int, ProductDetails],trainItemByPgProductDAO:ItemByPgProductDAO,
    avgLogWeeklySaleByClientDAO: AvgLogWeeklySaleDAO, trainItemDAO: ItemByProductDAO) 
    extends LinkedDemandModel(productMap,trainItemByPgProductDAO,avgLogWeeklySaleByClientDAO,trainItemDAO){
  
   def predictProductDemand(productId: Int, productItems: Seq[Item]): Seq[(Item, Double)] = ???
}