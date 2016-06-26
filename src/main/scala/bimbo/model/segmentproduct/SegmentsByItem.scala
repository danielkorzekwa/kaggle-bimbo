package bimbo.model.segmentproduct

import bimbo.data.Item
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger

case class SegmentsByItem(items: Seq[Item]) extends LazyLogging {

  private def getKey(item: Item) = (item.depotId, item.routeId)

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

    val sortedItems = items.sortBy { item => (item.depotId, item.routeId, item.clientId) }
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
      lastKey = itemKey

      (itemKey, segmentId)

    }
    logger.info("Number of segments:" + (segmentId + 1))

    scala.collection.mutable.Map(segmentsByKey: _*)
  }
}