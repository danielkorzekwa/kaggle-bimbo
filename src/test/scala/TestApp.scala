

import breeze.linalg.DenseVector
import dk.bayes.math.gaussian.Gaussian
import dk.gp.gpr.GprModel
import dk.gp.cov.CovSEiso
import breeze.numerics._
import dk.gp.gpr.gpr
import dk.gp.gpr.predict
import breeze.linalg.DenseMatrix
import dk.bayes.math.accuracy.rmse
import dk.gp.mtgpr.mtgprTrain
import dk.gp.mtgpr.MtGprModel
import com.typesafe.scalalogging.slf4j.LazyLogging

object TestApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val noise = Gaussian(0, 10)
    val x = DenseVector.rangeD(0, 200, 1)
    val y = x.map(x => x + noise.draw())

    val model = GprModel(x.toDenseMatrix.t, y, CovSEiso(), DenseVector(log(1), log(1)), log(1))

    val trainedModel = gpr(x.toDenseMatrix.t, y, CovSEiso(), DenseVector(log(1), log(1)), log(1))

    val predicted = predict(x.toDenseMatrix.t, trainedModel)(::, 0)

    /**
     * Train mtgpr
     */
    logger.info("Training mtgpr...")
    val trainData = (0 until x.size).map { i =>
      val mean = predicted(i)

      val xPoint = DenseMatrix(x(i))
      val yPoint = DenseVector(y(i)) - mean

      DenseMatrix.horzcat(xPoint, yPoint.toDenseMatrix.t)
    }

    val covFunc = CovSEiso()
    val covFuncParams = DenseVector(log(1), log(1))
    val noiseLogStdDev = log(1)
    val mtGrpModel = MtGprModel(trainData, covFunc, covFuncParams, noiseLogStdDev)
    val trainedMtGrpModel = mtgprTrain(mtGrpModel)

    val predicted2 = (0 until x.size).map { i =>
      val xPoint = DenseMatrix(x(i))
      val yPoint = DenseVector(y(i))
      val mean = predicted(i)
      val covFunc = CovSEiso()
      val covFuncParams = DenseVector(log(1), log(1))
      val noiseLogStdDev = log(1)

      val model = GprModel(xPoint, yPoint, covFunc, covFuncParams, noiseLogStdDev, mean)
      val predictedPoint = predict(xPoint, model)(0, 0)
      predictedPoint
    }.toArray

    val predicted2Trained = (0 until x.size).map { i =>
      val xPoint = DenseMatrix(x(i))
      val yPoint = DenseVector(y(i))
      val mean = predicted(i)
      val covFunc = CovSEiso()

      val covFuncParams = trainedMtGrpModel.covFuncParams
      val noiseLogStdDev = trainedMtGrpModel.likNoiseLogStdDev

      val model = GprModel(xPoint, yPoint, covFunc, covFuncParams, noiseLogStdDev, mean)
      val predictedPoint = predict(xPoint, model)(0, 0)
      predictedPoint
    }.toArray

    println(DenseMatrix.horzcat(x.toDenseMatrix.t, y.toDenseMatrix.t, predicted.toDenseMatrix.t, DenseMatrix(predicted2).t, DenseMatrix(predicted2Trained).t).toString(100, 100))

    val a = 0 
    val b = 50
    println("rmse1=" + rmse(log(x(a to b)+1.0), log(predicted(a to b)+1.0)))
    println("rmse2=" + rmse(log(x(a to b)+1.0), log(DenseVector(predicted2)(a to b)+1.0)))
    println("rmse2Trained=" + rmse(log(x(a to b)+1.0), log(DenseVector(predicted2Trained)(a to b)+1.0)))
  }

}