package bimbo.data.dao

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging

import bimbo.data.Item
import bimbo.data.dao.allitems.AllItemsDAO
import dk.gp.util.loadObject
import dk.gp.util.saveObject

case class ItemByDepotDAO(allItemsDAO: AllItemsDAO) extends LazyLogging {

  private lazy val allItems = {
    logger.info("Loading items...")
    val items = allItemsDAO.getAllItems()
    logger.info("Loading items...done")
    items
  }

  def getDepotItems(depotId: Int): Seq[Item] = {

    val depotItemsFile = getDepotItemsFileName(depotId)
    if (!new File(depotItemsFile).exists()) {

      logger.info("Getting items for depot...")
      val depotItems = allItems.filter { item => item.depotId == depotId }
      logger.info("Getting items for depot...done")
      saveObject(depotItems, depotItemsFile)
    }

    val depotItems = try {
      loadObject[List[Item]](depotItemsFile)
    } catch {
      case e: Exception => {
        logger.info("Loading items for depot:%d failed".format(depotId))
        throw e
      }
    }
    depotItems
  }

  private def getDepotItemsFileName(depotId: Int): String = {
    val dsName = new File(allItemsDAO.itemsFile).getName
    val baseName = "target/kryo/" + dsName

    baseName + "_depotItems_%d.kryo".format(depotId)
  }
}