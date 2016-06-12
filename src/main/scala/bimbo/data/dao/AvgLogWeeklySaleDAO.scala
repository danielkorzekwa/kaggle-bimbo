package bimbo.data.dao

import breeze.linalg._
import java.io.File

case class AvgLogWeeklySaleDAO() {

  private val avgLogWeeklySaleByClient: Map[Int, Double] = loadAvgLogWeeklySale()

  def getAvgLogWeeklySaleForClient(clientId: Int): Option[Double] = avgLogWeeklySaleByClient.get(clientId)

  private def loadAvgLogWeeklySale(): Map[Int, Double] = {
    val clientAvgLogWeeklySale = csvread(new File("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale.csv"), skipLines = 1)
    val avgLogWeeklySaleByClient: Map[Int, Double] = (0 until clientAvgLogWeeklySale.rows).map(i => clientAvgLogWeeklySale(i, 0).toInt -> clientAvgLogWeeklySale(i, 1)).toMap
    avgLogWeeklySaleByClient
  }
}