package bimbo.model.segmentproduct

import bimbo.data.Item
import com.typesafe.scalalogging.slf4j.LazyLogging

case class SegmentsByItem(items: Seq[Item]) extends LazyLogging{

  //key (depot,route)
  private val segmentsByKey: Map[(Int, Int), Int] = computeSegmentsByKey(items)
  def getSegment(item: Item): Int = {
    segmentsByKey.get((item.depotId, item.routeId)) match {
      case Some(segmentId) => segmentId
      case None => {
        val itemKey = segmentsByKey.keys.find{case (depotId,routeId) => depotId==item.depotId}.getOrElse(segmentsByKey.keys.head)
        segmentsByKey(itemKey)
      }
    }
  }

  private def computeSegmentsByKey(items: Seq[Item]): Map[(Int, Int), Int] = {

    val sortedItems = items.sortBy { item => (item.depotId, item.routeId) }
    var segmentId = 0
    var segmentSize = 0
    var lastKey = (0, 0) //depotId,routeId
    val segmentsByKey: Map[(Int, Int), Int] = sortedItems.map { item =>

      val itemKey = (item.depotId, item.routeId)
      if (segmentSize > 100 && !itemKey.equals(lastKey)) {
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