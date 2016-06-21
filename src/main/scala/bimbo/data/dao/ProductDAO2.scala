package bimbo.data.dao

import bimbo.data.ProductDetails
import scala.io.Source
import java.io.File
import bimbo.data.PgProductDetails
import bimbo.data.GenericProductDetails
import bimbo.data.PgProductDetails

case class ProductDAO2(productsFile: String) {

  def getProductMap(): Map[Int,ProductDetails] = {

    val productMap:Map[Int,ProductDetails] = Source.fromFile(new File(productsFile)).getLines().drop(1).map { l =>

      val PgProduct = "(\\d+),(.+) (\\d+)p (\\d+)g .+".r
       val GProduct = "(\\d+),(.+) (\\d+)g .+".r
      val GenericProduct = "(\\d+),(.+)".r

      val productDetails = l match {
        case PgProduct(id, name, p, g)       => id.toInt  -> PgProductDetails( name, p.toInt, g.toInt)
        case GProduct(id,name,g) => id.toInt -> PgProductDetails(name,1,g.toInt)
        case GenericProduct(id, description) => id.toInt -> GenericProductDetails(description)
      }
     productDetails
    }.toMap

    productMap
  }
}