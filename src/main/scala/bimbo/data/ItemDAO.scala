package bimbo.data

import java.io.File
import dk.gp.util.saveObject
import dk.gp.util.loadObject

case class ItemDAO(bimboItemDS: ItemDS) {

  init()

  private def init() = {

    if (!new File(getProductsFileName).exists()) {

      val items = bimboItemDS.getAllItems()
      val itemsByProduct = items.groupBy { i => i.productId }

      val productList = itemsByProduct.keys.toList
      saveObject(productList, getProductsFileName)

      itemsByProduct.foreach {
        case (productId, items) =>
          saveObject(items, getProductItemsFileName(productId))
      }
    }
  }

  def getProductItems(productId: Int): Seq[Item] = {
    if (new File(getProductItemsFileName(productId)).exists())
      loadObject[List[Item]](getProductItemsFileName(productId))
    else List()

  }

  private def getProductsFileName(): String = {
    val dsName = new File(bimboItemDS.getDSFile()).getName
    val baseName = "target/kryo/" + dsName
    val productListFileName = baseName + "_productList.kryo"
    productListFileName
  }

  private def getProductItemsFileName(productId: Int): String = {
    val dsName = new File(bimboItemDS.getDSFile()).getName
    val baseName = "target/kryo/" + dsName

    baseName + "_productItems_%d.kryo".format(productId)
  }
}