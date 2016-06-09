package bimbo

import com.typesafe.scalalogging.slf4j.LazyLogging

import bimbo.data.CSVBimboTestItemDS
import bimbo.model.baseline.BaselineModel
import breeze.linalg._
import dk.gp.util.csvwrite

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    logger.info("Generating submission...")

    val testItems = CSVBimboTestItemDS("c:/perforce/daniel/bimbo/segments/train_9.csv").getAllItems()
    val model = BaselineModel()

    val idColumn = DenseVector.rangeD(0, testItems.size, 1)
    val predictedDemand = model.predict(testItems)

    val predictionMat = DenseVector.horzcat(idColumn, predictedDemand)
    csvwrite("target/submission.csv", predictionMat, header = "id,Demanda_uni_equil")

    logger.info("Generating submission...done")
  }

}