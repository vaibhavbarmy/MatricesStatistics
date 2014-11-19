/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 11/20/14
 * Time: 1:57 AM
 * To change this template use File | Settings | File Templates.
 */
import scala.math.log10
import scala.math.pow
object test {
  
  def main(args: Array[String]){

    val num:Double=1034.345;
    val exponent = num.toInt
    val mantissa = (num - exponent).toFloat

    println(exponent, mantissa.toString.split("\\.")(1))


  }

}
