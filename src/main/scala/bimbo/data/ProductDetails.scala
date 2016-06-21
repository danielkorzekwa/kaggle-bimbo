package bimbo.data

sealed abstract class ProductDetails
case class PgProductDetails(name:String,p:Int,g:Int) extends ProductDetails
case class GenericProductDetails(description:String) extends ProductDetails