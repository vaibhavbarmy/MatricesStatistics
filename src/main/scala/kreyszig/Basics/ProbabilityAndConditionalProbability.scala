package kreyszig.Basics

import scala.collection.mutable
import BaseAlgorithms._
/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 12/6/14
 * Time: 10:27 PM
 * To change this template use File | Settings | File Templates.
 */
object ProbabilityAndConditionalProbability {
  abstract class Probability

  /**
   * case class having information specific to a data point
   *
   * @param event   value of the data point or event
   * @param outcome function to calculate outcome at that data point
   * @param pdf     function to calculate pdf
   * @tparam T
   */
  case class eventOutcomeProbabilityDensityFunctions[T](event: T, outcome:T => T, pdf: T => Double) extends Probability

  /**
   * case class having probability and outcome information specific to a data point
   *
   * @param event       value of the data point or event
   * @param outcome     outcome value at that data point
   * @param probability probability at that data point
   * @tparam T
   */
  case class eventOutcomeProbability[T](event: T, outcome: Option[T], probability: Option[Double]) extends Probability

  /**
   * method to get distinct list of eventOutcomeProbability class based on distinct event points
   *
   * @param outcomeAdditionFunc           outcome addition function required in case of a data point occuring more than twice
   * @param listOfEventOutcomeProbability list of data points of events
   * @tparam T
   * @return
   */
  def distinct[T](outcomeAdditionFunc: (T,T) => T, listOfEventOutcomeProbability: List[eventOutcomeProbability[T]]): List[eventOutcomeProbability[T]] = {
    val mapOfEventWithLocation: mutable.LinkedHashMap[T,Int] = mutable.LinkedHashMap[T,Int]()
    def iter(acc: Array[eventOutcomeProbability[T]], currentList: List[eventOutcomeProbability[T]]): Array[eventOutcomeProbability[T]] = {
      if(currentList.isEmpty) acc
      else {
        val head: eventOutcomeProbability[T] = currentList.head
        val tail: List[eventOutcomeProbability[T]] = currentList.tail
        val event:T = head.event
        if(mapOfEventWithLocation.contains(event)) {
          val location: Int = mapOfEventWithLocation.get(event).get
          //println(location)
          val previousEOP: eventOutcomeProbability[T] = acc(location)
          val tempAcc = acc
          tempAcc.update(location, eventOutcomeProbability(previousEOP.event, Some(outcomeAdditionFunc(previousEOP.outcome.get, head.outcome.get)), Some(previousEOP.probability.getOrElse(0.0) + head.probability.getOrElse(0.0))))
          iter(tempAcc, tail)
        }
        else {
          val currentLocation = if (mapOfEventWithLocation.nonEmpty) mapOfEventWithLocation.last._2 + 1 else 0
          mapOfEventWithLocation.put(head.event, currentLocation)
          val updatedAcc: Array[eventOutcomeProbability[T]] = acc :+ eventOutcomeProbability(head.event, head.outcome, head.probability)
          iter(updatedAcc, tail)
        }
      }
    }
    iter(Array[eventOutcomeProbability[T]](), listOfEventOutcomeProbability).toList
  }

  /**
   * method to calculate individual outcome and probability value based on the input outcome and pdf function present in case class eventOutcomeProbabilityDensityFunctions[T]
   *
   * @param eventOutcomePdfs list of all event outcome functions and pdfs
   * @tparam T
   * @return
   */
  def probabilityCalculator[T](eventOutcomePdfs: List[eventOutcomeProbabilityDensityFunctions[T]]): List[eventOutcomeProbability[T]] ={
    def iter(acc: List[eventOutcomeProbability[T]], currentEventOutcomePdfs: List[eventOutcomeProbabilityDensityFunctions[T]]) : List[eventOutcomeProbability[T]] = {
      if (currentEventOutcomePdfs.isEmpty) acc
      else {
        val head: eventOutcomeProbabilityDensityFunctions[T] = currentEventOutcomePdfs.head
        val tail: List[eventOutcomeProbabilityDensityFunctions[T]] = currentEventOutcomePdfs.tail
        iter(acc :+ eventOutcomeProbability(head.event, Some(head.outcome(head.event)), Some(head.pdf(head.event))), tail)
      }
    }
    iter(List[eventOutcomeProbability[T]](), eventOutcomePdfs)
  }

  /**
   * method to get normalized probability for list of data points or events based on input sample space
   *
   * @param eventOutcomePdfs        list of event outcome func and pdfs class
   * @param sampleSpaceOutcomePdfs  list of sample space outcome func and pdfs
   * @tparam T
   * @return
   */
  def normalizedProbabilityCalculator[T](eventOutcomePdfs: List[eventOutcomeProbabilityDensityFunctions[T]], sampleSpaceOutcomePdfs: List[eventOutcomeProbabilityDensityFunctions[T]]): List[eventOutcomeProbability[T]] ={
    var sampleProbability: Double = 0.0
    probabilityCalculator(sampleSpaceOutcomePdfs).foreach(f => {
      if (f.probability.getOrElse(0.0) > 0) {sampleProbability += f.probability.getOrElse(0.0)} else sampleProbability
    })
    probabilityCalculator(eventOutcomePdfs).map(f => new eventOutcomeProbability(f.event, f.outcome, Option(f.probability.getOrElse(0.0)/sampleProbability)))
  }

  /**
   * method to get the sum of all the probability values for all data points present in a given event
   *
   * @param eventOutcomeProbabilities list of data point , outcome, probability values case class
   * @tparam T
   * @return
   */
  def normalizedProbabilityValueCalculator[T](eventOutcomeProbabilities: List[eventOutcomeProbability[T]]): Double = {
    var probability: Double = 0.0
    eventOutcomeProbabilities.foreach(f => if (f.probability.getOrElse(0.0) > 0.0) probability += f.probability.getOrElse(0.0))
    probability
  }

  def main(args: Array[String]){
    var eopdfs: List[eventOutcomeProbabilityDensityFunctions[Int]] = List[eventOutcomeProbabilityDensityFunctions[Int]]()
    var sopdfs: List[eventOutcomeProbability[Int]] = List[eventOutcomeProbability[Int]]()
    for (x <- 0 until 20) sopdfs = sopdfs :+ eventOutcomeProbability(x, Option(x*2), Option(1.0/20.0))
    val count = normalizedProbabilityValueCalculator[Int](sopdfs)
    println(count)
    for (x <- 0 until 5) eopdfs = eopdfs :+ eventOutcomeProbabilityDensityFunctions[Int](x, x => x*2, x => 1/20.0)
    println(normalizedProbabilityValueCalculator[Int](probabilityCalculator[Int](eopdfs)))
    val x = Array(1,2,3)
    sopdfs = sopdfs :+ eventOutcomeProbability(1, Some(1*3), Some(1.0/20.0))
    //println(distinct[Int]((x,y) => x+y, sopdfs))
    //println((x :+ 4).length)
    val a: eventOutcomeProbability[Int] = eventOutcomeProbability(1,Some(2),Some(.5))
    val b: eventOutcomeProbability[Int] = eventOutcomeProbability(1,Some(2),Some(.4))
    val c: eventOutcomeProbability[Int] = eventOutcomeProbability(1,Some(2),Some(.4))
    val d: eventOutcomeProbability[Int] = eventOutcomeProbability(1,Some(2),Some(.6))

    println(union(List(a,b), List(c,d)))
  }
}
