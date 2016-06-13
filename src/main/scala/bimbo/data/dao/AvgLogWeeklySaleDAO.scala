package bimbo.data.dao

import breeze.linalg._
import java.io.File

case class AvgLogWeeklySaleDAO(dataFile:String) {

  private val avgLogWeeklySaleByClient: Map[Int, Double] = loadAvgLogWeeklySale()

  def getAvgLogWeeklySaleForClient(clientId: Int): Option[Double] = avgLogWeeklySaleByClient.get(clientId)

  private def loadAvgLogWeeklySale(): Map[Int, Double] = {
    val clientAvgLogWeeklySale = csvread(new File(dataFile), skipLines = 1)
    val avgLogWeeklySaleByClient: Map[Int, Double] = (0 until clientAvgLogWeeklySale.rows).map{i =>
      
      val clientId = clientAvgLogWeeklySale(i, 0).toInt 
      val logAvgSale = "%.1f".format(clientAvgLogWeeklySale(i, 1)).toDouble
      
      clientId -> logAvgSale
  }.toMap
    avgLogWeeklySaleByClient
  }
}