package bimbo

import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.model.clientproductgp.priordemand.createSalesDemandData
import dk.gp.gpr.GprModel
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import breeze.numerics._
import dk.gp.gpr.gpr
import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.gpr.predict
import breeze.linalg.DenseMatrix
import dk.gp.mtgpr.mtgprTrain
import dk.gp.mtgpr.MtGprModel
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.model.clientproductgp.RouteCovFunc
import dk.gp.cov.CovSEiso
import bimbo.data.dao.ItemByProductDAO
import bimbo.data.dao.ItemByPgProductDAO
import bimbo.data.dao.ProductDAO
import bimbo.data.PgProductDetails

object TrainLinkedModelPriorApp extends LazyLogging {

   val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
     val productMap = ProductDAO("c:/perforce/daniel/bimbo/producto_tabla.csv").getProductMap()

   
    val trainItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_8.csv", clientNamesDAO)
 val trainItemByPgProductDAO = ItemByPgProductDAO(trainItemsDAO, productMap)
   
  val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_8.csv")

  def main(args: Array[String]): Unit = {

    trainGrp()
  }

 

  def trainGrp() = {
    val items = trainItemByPgProductDAO.getProductItems(PgProductDetails("Gansito",1,50))//.filter { item => item.productId==43285 }
    val (x, y) = createSalesDemandData(items, avgLogWeeklySaleByClientDAO)

    logger.info("Data size:" + x.rows)

    val gprModel = gpr(x, y, CovSEiso(), DenseVector(log(1), log(1)),log(1))
    //  val gprModel = GprModel(x, y, CovSEiso(), DenseVector(log(1), log(1)), log(1))
    println("covFuncParams=%s, noiseLogStdDev=%f".format(gprModel.covFuncParams, gprModel.noiseLogStdDev))

    val xTest = DenseVector.rangeD(0, 17, 1).toDenseMatrix.t
    val predicted = exp(predict(xTest, gprModel)(::, 0)) - 1.0

    println(DenseMatrix.horzcat(xTest, predicted.toDenseMatrix.t))
  }
}