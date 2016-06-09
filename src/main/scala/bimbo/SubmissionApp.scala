package bimbo

import com.typesafe.scalalogging.slf4j.LazyLogging
import bimbo.data.CSVBimboTestItemDS
import bimbo.model.baseline.BaselineModel
import breeze.linalg._
import dk.gp.util.csvwrite
import bimbo.data.CSVBimboItemDS
import bimbo.model.groupby.GroupByModel

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val now = System.currentTimeMillis()
    logger.info("Generating submission...")

    logger.info("Loading test set...")
    val testItems = CSVBimboItemDS("c:/perforce/daniel/bimbo/segments/train_9.csv").getAllItems()

    logger.info("Loading train set...")
    val trainItems = CSVBimboItemDS("c:/perforce/daniel/bimbo/segments/train_8.csv").getAllItems()

    logger.info("Predicting demand...")
    val model = GroupByModel(trainItems)
    val predictedDemand = model.predict(testItems)

    logger.info("Saving submission...")
    val idColumn = DenseVector.rangeD(0, testItems.size, 1)
    val predictionMat = DenseVector.horzcat(idColumn, predictedDemand)
    csvwrite("target/submission.csv", predictionMat, header = "id,Demanda_uni_equil")

    logger.info("Generating submission...done:" + (System.currentTimeMillis() - now))
  }

}