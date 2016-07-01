package bimbo.model.knngp2.knn

import bimbo.data.Item

trait Knn {
  
   def getKNN(item:Item,k:Int):Seq[Item] 
  
}