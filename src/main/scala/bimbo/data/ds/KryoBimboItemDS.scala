package bimbo.data.ds

import dk.gp.util.loadObject
import bimbo.data.Item
import bimbo.data.ds.ItemDS

case class KryoBimboItemDS(bimboFile: String) extends ItemDS {

  private val items = loadObject[List[Item]](bimboFile)

  def getAllItems(): Seq[Item] = {
    items
  }
  
   def getDSFile():String = bimboFile
}