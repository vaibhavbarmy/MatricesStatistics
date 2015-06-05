package kreyszig.Basics

import scala.math.{pow, E}
import MathematicalOperators.factorial
/**
 * Created with IntelliJ IDEA.
 * User: Vaibhav
 * Date: 5/6/15
 * Time: 3:09 PM
 * To change this template use File | Settings | File Templates.
 */
object Poisson {

  /**
   * Method to return the probability density function of poisson distribution of mean u = n*p and variance u
   *
   * @return
   */
  def pdfPoisson: (Double, Int) => Double = (u: Double, x: Int) => pow(u, x)*pow(E, (-1*u))/factorial(x)

  def main(args: Array[String]){
    println(pdfPoisson(2.0,0)+pdfPoisson(2.0,1)+pdfPoisson(2.0,2)+pdfPoisson(2.0,3))
    //Exercise 24.7
    //5.
    println(pdfPoisson(5.0,0)+pdfPoisson(5.0,1)+pdfPoisson(5.0,2)+pdfPoisson(5.0,3))
    //7.
    println(1-(pdfPoisson(0.5,0)+pdfPoisson(0.5,1)))
    println(pow(0.9,29))
    println(1-pdfPoisson(0.2  ,0))
    var answer: Double = 0.0
    for (x<- 1 until 11){answer += pdfPoisson(0.1, x)}
    println(1- answer)
  }

}
