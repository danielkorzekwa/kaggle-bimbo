package bimbo.data.dao

import bimbo.data.PgProductDetails
import bimbo.data.Item
import bimbo.data.dao.allitems.AllItemsDAO
import java.io.File
import dk.gp.util.loadObject
import bimbo.data.ProductDetails
import dk.gp.util.saveObject
import com.typesafe.scalalogging.slf4j.LazyLogging

case class ItemByPgProductDAO(allItemsDAO: AllItemsDAO, productMap: Map[Int, ProductDetails]) extends LazyLogging {

  private lazy val allItems = {
    logger.info("Loading items...")
    val items = allItemsDAO.getAllItems()
     logger.info("Loading items...done")
     items
  }

  def getProductItems(product: PgProductDetails): Seq[Item] = {

    val productItemsFile = getProductItemsFileName(product)
    if (!new File(productItemsFile).exists()) {

      logger.info("Getting items for productDetails...")
      val productItems = allItems.filter { item => productMap(item.productId).equals(product) }
      logger.info("Getting items for productDetails...done")
      saveObject(productItems, productItemsFile)
    }

    val productItems = try {
      loadObject[List[Item]](productItemsFile)
    }
    catch {
      case e :Exception => {
        logger.info("Loading items for product:%s failed".format(product))
            throw e
      }
    }
    productItems
  }

  private def getProductItemsFileName(product: PgProductDetails): String = {
    val dsName = new File(allItemsDAO.itemsFile).getName
    val baseName = "target/kryo/" + dsName

    baseName + "_productItems_%s.kryo".format(product.toString())
  }
}