package bimbo

import dk.gp.util.loadObject
import dk.gp.util.saveObject
import bimbo.data.CSVBimboItemDS

object SerialiseKryoApp {

  def main(args: Array[String]): Unit = {

    val inputFile = "c:/perforce/daniel/bimbo/segments/train_9"
    val items = CSVBimboItemDS(inputFile + ".csv").getAllItems()
    saveObject(items, inputFile + ".kryo")

  }
}