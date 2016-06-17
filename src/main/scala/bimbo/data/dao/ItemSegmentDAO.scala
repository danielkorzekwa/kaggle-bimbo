package bimbo.data.dao

import bimbo.data.Item
import dk.gp.util.loadObject

case class ItemSegmentDAO(segmentsFile:String) {

  /**
   * key - (product,client)
   * value - segmentId
   */
  val segmentByProductClient: Map[(Int, Int), Int] = loadObject[Map[(Int, Int), Int]](segmentsFile)

  def getSegment(item: Item): Int = {
    segmentByProductClient.get((item.productId, item.clientId)) match {
      case Some(segmentId) => segmentId
      case None => {
        val productClientKey = segmentByProductClient.keys.find{case (productId,clientId) => productId==item.productId}.get
        segmentByProductClient(productClientKey)
      }
    }
  }
}