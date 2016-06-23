package bimbo

import bimbo.model.clientproductgp.trainClientProductGPModel
import bimbo.data.dao.AvgLogWeeklySaleDAO
import bimbo.data.dao.allitems.AllTrainItemsDAO
import bimbo.data.dao.ClientNamesDAO
import bimbo.data.dao.ItemByProductDAO

object TrainClientProductGPApp {

  def main(args: Array[String]): Unit = {

    val clientNamesDAO = ClientNamesDAO("c:/perforce/daniel/bimbo/cliente_tabla.csv")
    val allItemsDAO = AllTrainItemsDAO("c:/perforce/daniel/bimbo/segments/train_8.csv", clientNamesDAO)
    val itemDAO = ItemByProductDAO(allItemsDAO)

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

    val items = itemDAO.getProductItems(43285)

    val (covFuncParams, noiseLogStdDev) = trainClientProductGPModel(items, avgLogWeeklySaleByClientDAO,null)

    println("covFuncParams=%s, noiseLogStdDev=%f".format(covFuncParams, noiseLogStdDev))

  }
}