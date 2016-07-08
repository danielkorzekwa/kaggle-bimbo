

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
import bimbo.data.dao.AvgLogDemandByClientDAO
import dk.gp.gpr.gprPredict
import bimbo.data.PgProductDetails
import breeze.stats._

object TestApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

   println( PgProductDetails("a",32,4).hashCode()== PgProductDetails("a",32,4).hashCode())
  
   val g = dk.bayes.math.gaussian.Gaussian(1.5, 1)
   val truncGaussian = g.truncate(0.0, true)
   
val v = DenseVector.tabulate(10000)(i => truncGaussian.draw)
println(mean(v))
  }
  

}