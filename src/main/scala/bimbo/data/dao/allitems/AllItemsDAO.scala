package bimbo.data.dao.allitems

import bimbo.data.Item

trait AllItemsDAO {
  
   def getAllItems(): Seq[Item]
   val itemsFile:String
}