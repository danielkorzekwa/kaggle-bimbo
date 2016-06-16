

import dk.gp.util.saveObject
import breeze.linalg.DenseVector
import dk.gp.util.loadObject
import breeze.linalg.DenseMatrix
import dk.bayes.math.linear.isIdentical
import org.apache.spark.serializer.KryoSerializer
import org.apache.spark.SparkConf
import java.io.FileOutputStream
import com.esotericsoftware.kryo.io.Output
import java.io.FileInputStream
import com.esotericsoftware.kryo.io.Input
import com.twitter.chill.ScalaKryoInstantiator

object KryoTestApp {
  
  def main(args: Array[String]): Unit = {
    
    val v = DenseVector(3.0,4.0)
     
    val conf = new SparkConf()
      .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    //  .set("spark.kryo.registrator", classOf[MyRegistrator].getName)
      .set("spark.kryo.referenceTracking", "false")
      .set("spark.kryoserializer.buffer.mb", "8")
   val kryo = new KryoSerializer(conf).newKryo()
    val fileOut = new FileOutputStream("target/test.kryo2")
    val output = new Output(fileOut)
    kryo.writeClassAndObject(output, v)
    output.close()
    
    
   
      val fileIn = new FileInputStream("target/test.kryo2")
    val input = new Input(fileIn)
       //val deser = kryo.readObject(input, classOf[DenseVector[Double]])
   val deser = kryo.readClassAndObject(input).asInstanceOf[DenseVector[Double]]
     println(deser)
   
  }
}