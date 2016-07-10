package bimbo.data.dao

import java.io.File
import breeze.linalg._

case class AvgLogPriceByProductDAO(avgPriceFile: String) {

  private val avgLogPriceByProductMap: Map[Int, Double] = loadAvgLogPrice()

  def getAvgLogPrice(productId: Int): Option[Double] = {
    avgLogPriceByProductMap.get(productId)
  }

  private def loadAvgLogPrice(): Map[Int, Double] = {
    val avgLogPriceData = csvread(new File(avgPriceFile), skipLines = 1)
    val avgLogPriceByProductMap: Map[Int, Double] = (0 until avgLogPriceData.rows).map { i =>

      val productId = avgLogPriceData(i, 0).toInt
      val logAvgPrice = avgLogPriceData(i, 1)

      productId -> logAvgPrice
    }.toMap
    avgLogPriceByProductMap
  }
}