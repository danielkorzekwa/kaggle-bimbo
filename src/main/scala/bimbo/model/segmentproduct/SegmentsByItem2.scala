package bimbo.model.segmentproduct

import bimbo.data.Item
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import bimbo.data.dao.AvgLogWeeklySaleDAO

case class SegmentsByItem2(items: Seq[Item], avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO) extends LazyLogging {

  private def getKey(item: Item) = {
      val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(5.54149)
      clientLogSale
  }

  //key (depot,route)
  private val segmentsByKey = computeSegmentsByKey(items)

  def addItemSegment(item: Item, segmentId: Int) = this.synchronized {
    segmentsByKey += getKey(item) -> segmentId
  }

  def getSegment2(item: Item): Option[Int] = this.synchronized {
    segmentsByKey.get(getKey(item))
  }

  def getSegmentIds(): Seq[Int] = this.synchronized { segmentsByKey.values.toList }

  private def computeSegmentsByKey(items: Seq[Item]) = {

    val sortedItems = items.sortBy { item => 
      val clientLogSale = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).getOrElse(5.54149)  
      clientLogSale
    }
    var segmentId = 0
    var segmentSize = 0
    var lastKey = 0d //depotId,routeId
    var lastClientId=0
    val segmentsByKey = sortedItems.map { item =>

      val itemKey = getKey(item)
      if (segmentSize > 200 && !itemKey.equals(lastKey) && item.clientId!=lastClientId) {
        segmentId += 1
        segmentSize = 0
      }

        lastKey = itemKey
      lastClientId=item.clientId
      segmentSize += 1

      (itemKey, segmentId)

    }
   

   val segmentMap = scala.collection.mutable.Map(segmentsByKey: _*)
    logger.info("Number of segments:" + (segmentId + 1) + ", segmentMapSize=" + segmentMap.values.toList.distinct.size)
   segmentMap
  }
}