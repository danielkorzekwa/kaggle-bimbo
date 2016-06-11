package bimbo.data

case class ItemDAO(bimboItemDS: ItemDS) {

  private val items = bimboItemDS.getAllItems()

  def getProductItems(productId: Int): Seq[Item] = {
    items.filter(i => i.productId == productId)
  }
}