package bimbo.model.clientproductgp.priordemand

import java.io.File
import bimbo.data.Item
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.GprModel
import dk.gp.cov.CovSEiso
import dk.gp.gpr.gprPredict
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.util.StatCounterByKey
import breeze.stats._
import bimbo.data.dao.AvgLogDemandByClientDAO

/**
 * Predicts demand from client average sales
 */
case class PriorLogDemandModel(items: Seq[Item], avgLogWeeklySaleDAO: AvgLogWeeklySaleDAO,avgLogDemandDAO:AvgLogDemandByClientDAO) {

  

  private val itemsSize = items.size

  private val priorLogDemand = if (items.size == 0) log(7d + 1) else {
    items.map(i => log(i.demand + 1)).sum / items.size
  }

  lazy val gprModel = {

    //val x = DenseMatrix(items.map(item => avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(item.clientId).get).toArray).t
    //   val x = DenseMatrix(items.map(item => avgLogDemandDAO.getAvgLogDemand(item.clientId).get).toArray).t

    //val y = DenseVector(items.map(item => log(item.demand + 1)).toArray)

 //     val (x,y) = createAvgLogDemandData(items, avgLogDemandDAO)
    val (x, y) = createSalesDemandData(items, avgLogWeeklySaleDAO)
    GprModel(x, y, CovSEiso(), DenseVector(log(1), log(1)), log(1))
    //   GprModel(x, y, CovSEiso(), DenseVector(2.167843777084265, 2.5015536901699758), -1.375299,mean(y))
  }

  def predictLogDemand(item: Item): Double = {

    predictLogDemand(item.clientId)
  }
  
  def predictLogDemand(clientId:Int): Double = {
       val priorLogDemand2 = avgLogWeeklySaleDAO.getAvgLogWeeklySaleForClient(clientId) match {
      case Some(avgLogWeeklySale) if (itemsSize > 0) => gprPredict(DenseMatrix(avgLogWeeklySale), gprModel)(0, 0)
      case _                                         => priorLogDemand

    }

//         val priorLogDemand2 = avgLogDemandDAO.getAvgLogDemand(item.clientId) match {
//          case Some(avgLogDemand) if (itemsSize > 0) => predict(DenseMatrix(avgLogDemand), gprModel)(0, 0)
//          case _                                         => priorLogDemand
//    
//        }

    priorLogDemand2
  }
}