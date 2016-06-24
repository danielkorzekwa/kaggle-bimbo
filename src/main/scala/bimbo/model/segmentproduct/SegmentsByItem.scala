package bimbo.model.segmentproduct

import bimbo.data.Item
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger

case class SegmentsByItem(items: Seq[Item]) extends LazyLogging{

  def getKey(item:Item) = (item.depotId, item.routeId)
  
  //key (depot,route)
  private val segmentsByKey = computeSegmentsByKey(items)
  def getSegment(item: Item): Int = {
    val segmentId = segmentsByKey.get(getKey(item)) match {
      case Some(segmentId) => segmentId
      case None => {
        val itemKey = segmentsByKey.keys.find{case key => key._1==item.depotId}.getOrElse(segmentsByKey.keys.head)
        segmentsByKey(itemKey)
      }
    }
    segmentId
  }

  private def computeSegmentsByKey(items: Seq[Item]) = {

    val a = new AtomicInteger(0)
    val sortedItems = items.sortBy { item =>   (item.depotId, item.routeId,item.clientId)}
    var segmentId = 0
    var segmentSize = 0
    var lastKey = (0, 0) //depotId,routeId
    val segmentsByKey = sortedItems.map { item =>

      val itemKey = getKey(item)
      if (segmentSize > 200 && !itemKey.equals(lastKey)) {
        segmentId += 1
        segmentSize = 0
      }

      segmentSize += 1

      (itemKey, segmentId)

    }.toMap
    logger.info("Number of segments:" + (segmentId+1))
    segmentsByKey
  }
}