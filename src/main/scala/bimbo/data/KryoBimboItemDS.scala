package bimbo.data

import dk.gp.util.loadObject

case class KryoBimboItemDS(bimboFile: String) extends ItemDS {

  private val items = loadObject[List[Item]](bimboFile)

  def getAllItems(): Seq[Item] = {
    items
  }
}