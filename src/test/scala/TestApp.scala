

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import dk.gp.math.sqDist
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.AvgLogWeeklySaleDAO
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.gpr
import dk.gp.gpr.predict
import bimbo.data.dao.AvgLogDemandByClientDAO
object TestApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

     val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_8.csv", clientNamesDAO)
    val itemDAO = ItemByProductDAO(allItemsDAO)

  val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_8.csv")
  val avgLogDemandDAO = AvgLogDemandByClientDAO("c:/perforce/daniel/bimbo/stats/avgLogDemandByClient_8.csv")
   val items = itemDAO.getProductItems(46232)
   val x =  DenseMatrix(items.map(item => avgLogDemandDAO.getAvgLogDemand(item.clientId).get).toArray).t
  val y = DenseVector(items.map(item => log(item.demand+1)).toArray)


    val gprModel = gpr(x, y, CovSEiso(), DenseVector(log(1), log(1)),log(1))
    //  val gprModel = GprModel(x, y, CovSEiso(), DenseVector(log(1), log(1)), log(1))
    println("covFuncParams=%s, noiseLogStdDev=%f".format(gprModel.covFuncParams, gprModel.noiseLogStdDev))

    val xTest = DenseVector.rangeD(0, 17, 1).toDenseMatrix.t
    val predicted = exp(predict(xTest, gprModel)(::, 0)) - 1.0

    println(DenseMatrix.horzcat(xTest, predicted.toDenseMatrix.t))
  
  }

}