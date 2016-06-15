package bimbo.data.dao

import java.io.File
import dk.gp.util.saveObject
import dk.gp.util.loadObject
import bimbo.data.Item
import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.dao.allitems.AllItemsDAO

case class ItemDAO(allItemsDAO:AllItemsDAO) extends LazyLogging {

  init()

  private def init() = {

    if (!new File(getProductsFileName).exists()) {
      logger.info("Caching items by product on disk...")
      val items = allItemsDAO.getAllItems()
      val itemsByProduct = items.groupBy { i => i.productId }

      val productList = itemsByProduct.keys.toList
      saveObject(productList, getProductsFileName)

      itemsByProduct.foreach {
        case (productId, items) =>
          saveObject(items, getProductItemsFileName(productId))
      }
      logger.info("Caching items by product on disk...done")
    }

  }

  def getProductItems(productId: Int): Seq[Item] = {
    if (new File(getProductItemsFileName(productId)).exists())
      loadObject[List[Item]](getProductItemsFileName(productId))
    else List()

  }
  
  def getProductIds():Seq[Int] = loadObject[List[Int]](getProductsFileName)

  private def getProductsFileName(): String = {
    val dsName = new File(allItemsDAO.itemsFile).getName
    val baseName = "target/kryo/" + dsName
    val productListFileName = baseName + "_productList.kryo"
    productListFileName
  }

  private def getProductItemsFileName(productId: Int): String = {
    val dsName = new File(allItemsDAO.itemsFile).getName
    val baseName = "target/kryo/" + dsName

    baseName + "_productItems_%d.kryo".format(productId)
  }
}