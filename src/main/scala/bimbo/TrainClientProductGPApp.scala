package bimbo

import bimbo.data.dao.ItemDAO
import bimbo.data.ds.CSVBimboItemDS
import bimbo.model.clientproductgp.trainClientProductGPModel
import bimbo.data.dao.AvgLogWeeklySaleDAO

object TrainClientProductGPApp {

  def main(args: Array[String]): Unit = {

    val trainItemsDS = CSVBimboItemDS("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv")
    val itemDAO = ItemDAO(trainItemsDS)

    val avgLogWeeklySaleByClientDAO = AvgLogWeeklySaleDAO("c:/perforce/daniel/bimbo/stats/clientAvgLogWeeklySale_3_8.csv")

    val items = itemDAO.getProductItems(2233)

    val (covFuncParams, noiseLogStdDev) = trainClientProductGPModel(items, avgLogWeeklySaleByClientDAO)

    println("covFuncParams=%s, noiseLogStdDev=%f".format(covFuncParams, noiseLogStdDev))

  }
}