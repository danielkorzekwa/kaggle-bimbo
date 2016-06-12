package bimbo

import bimbo.data.ItemDAO
import bimbo.data.CSVBimboItemDS
import bimbo.model.clientproductgp.trainClientProductGPModel

object TrainClientProductGPApp {

  def main(args: Array[String]): Unit = {

    val trainItemsDS = CSVBimboItemDS("c:/perforce/daniel/bimbo/segments/train_3_to_8.csv")
    val itemDAO = ItemDAO(trainItemsDS)

    val items = itemDAO.getProductItems(42128)

    val (covFuncParams, noiseLogStdDev) = trainClientProductGPModel(items)

    println("covFuncParams=%s, noiseLogStdDev=%f".format(covFuncParams, noiseLogStdDev))

  }
}