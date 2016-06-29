package bimbo.model.segmentproduct.util

import org.junit._
import Assert._
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.Item

class calcNewProductMapTest {

  val clientNamesDAO = ClientNamesDAO("/mnt/bimbo/cliente_tabla.csv")
  val trainItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_3_to_8.csv", clientNamesDAO)
  val trainItemByProductDAO = ItemByProductDAO(trainItemsDAO)
  val trainItems = trainItemByProductDAO.getProductItems(43231)

  val testItemsDAO = AllTrainItemsDAO("/mnt/bimbo/segments/train_9.csv", clientNamesDAO)
  val testItemByProductDAO = ItemByProductDAO(testItemsDAO)
  val testItems = testItemByProductDAO.getProductItems(43231)

  @Test def test = {

    val items = trainItems ++ testItems

    //key (clientId,productId), value - true is client product has not been see yet before.
    val newProductMap: Map[Item, Boolean] = calcNewProductMap(items)

    val existingProduct = Item(9, 1346, 1, 1215, 2374386, "MARGARITA GARCIA PEREZ", 43231, 8.0)
    assertFalse(newProductMap(existingProduct))

    val newProduct = Item(9, 1387, 1, 1012, 408251, "SALVADOR SAUCEDO PENA", 43231, 2.0)
    assertTrue(newProductMap(newProduct))

  }
}
