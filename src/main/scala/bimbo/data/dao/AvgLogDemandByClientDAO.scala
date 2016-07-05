package bimbo.data.dao

import breeze.linalg._
import java.io.File

case class AvgLogDemandByClientDAO(dataFile:String) {

  private val avgLogDemandByClient: Map[Int, Double] = loadAvgLoDemand()

  def getAvgLogDemand(clientId: Int): Option[Double] = avgLogDemandByClient.get(clientId)

  private def loadAvgLoDemand(): Map[Int, Double] = {
    val avgLogDemandData = csvread(new File(dataFile), skipLines = 1)
    val avgLogDemandByClient: Map[Int, Double] = (0 until avgLogDemandData.rows).map{i =>
      
      val clientId = avgLogDemandData(i, 0).toInt 
      val logAvgSale = avgLogDemandData(i, 1)//"%.6f".format(avgLogDemandData(i, 1)).toDouble
      
      clientId -> logAvgSale
  }.toMap
    avgLogDemandByClient
  }
}