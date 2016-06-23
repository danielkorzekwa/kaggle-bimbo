package bimbo.model.segmentproduct

import bimbo.data.Item

case class SegmentsByItem(items:Seq[Item]) {
  
  def getSegment(item:Item):Int = 1
  
}