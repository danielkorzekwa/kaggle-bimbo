package bimbo

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import scala.collection._

object SparkTestApp {
  
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setMaster("local").setAppName("Simple Application")
     conf.set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    val sc = new SparkContext(conf)
    
    val keysWithValuesList = Array("foo=A", "foo=A", "foo=A", "foo=A", "foo=B", "bar=C", "bar=D", "bar=D")
val data = sc.parallelize(keysWithValuesList)
//Create key value pairs
val kv = data.map(_.split("=")).map(v => (v(0), v(1))).cache()
val initialSet = mutable.HashSet.empty[String]
val addToSet = (s: mutable.HashSet[String], v: String) => s += v
val mergePartitionSets = (p1: mutable.HashSet[String], p2: mutable.HashSet[String]) => p1 ++= p2

val uniqueByKey = kv.aggregateByKey(initialSet)(addToSet, mergePartitionSets)
  }
}