package bimbo.data.dao

import org.junit.Test
import bimbo.data.PgProductDetails
import bimbo.data.PgProductDetails
import bimbo.data.GenericProductDetails
import bimbo.data.ProductDetails
import bimbo.data.PgProductDetails

class ProductDAOTest {
  
  @Test def test():Unit ={
    val productDAO = ProductDAO2("c:/perforce/daniel/bimbo/producto_tabla.csv")
 
val products = productDAO.getProductMap().values.toList.filter(p => p.isInstanceOf[PgProductDetails])
println(products.size)
println(products.distinct.size)
    
 //   println(productDAO.getAllProducts().filter { p => p.isInstanceOf[PgProductDetails] }.size)
 // println(productDAO.getAllProducts().filter { p => p.isInstanceOf[PgProductDetails] }.map(p => p.asInstanceOf[PgP)
 
    
  }
}