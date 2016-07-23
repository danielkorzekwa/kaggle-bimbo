package bimbo.data.dao

import breeze.linalg._
import java.io.File

case class ReturnRatioDAO(returnRatioFile: String) {

  private val returnRatioByClient: Map[Int, Double] = loadReturnRatio()

  def getReturnRatio(clientId: Int): Option[Double] = {
    returnRatioByClient.get(clientId)
  }

  private def loadReturnRatio(): Map[Int, Double] = {
    val returnRatioData = csvread(new File(returnRatioFile), skipLines = 1)
    val returnRatioByClient: Map[Int, Double] = (0 until returnRatioData.rows).map { i =>

      val clientId = returnRatioData(i, 0).toInt
      val returnRatio = returnRatioData(i, 1)

      clientId -> returnRatio
    }.toMap
    returnRatioByClient
  }
}