package RandomCode

import org.joda.time._
import scala.collection.mutable
import scala.collection.immutable


/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 5/9/14
 * Time: 12:26 PM
 * @author Vaibhav Agrawal
 * @version 1.0
 * @define 
 * To change this template use File | Settings | File Templates.
 */


/**
 * Main object for this code is to get the minimum frequency between the
 * data timestamps .. ie either weekly , month , yearly etc
 */

object FrequencyIdentification {
  case class dataInfo(dateMap: Seq[(Int,Int)], position: Seq[(Int)]){}
  private val FREQUENCY_MAP = mutable.LinkedHashMap(0->"milliSeconds",1->"seconds",2->"minutes",3->"hours",4->"days",5->"weeks",6->"months",7->"years")
  private val FREQUENCY_UPPER_LIMIT = FREQUENCY_MAP.last._1
  /**
   * @author                Vaibhav Agarwal
   * @define                Function to compare the frequency of two datemap: which is a representation of timestamp
   * @param previousDateMap The previous dateMap in the list of timestamps
   * @param nextDateMap     The next dateMap in the list of timestamps
   * @return                The frequency(weeks, months etc) between the two date
   */
  def tailFrequencyComparator(previousDateMap: Seq[(Int,Int)], nextDateMap: Seq[(Int,Int)]):Int ={
    def iterator(oldDateMap: Seq[(Int,Int)], newDateMap: Seq[(Int,Int)], result: Int): Int = {
      if(!oldDateMap.tail.isEmpty) {
        if(oldDateMap.last._2.equals(newDateMap.last._2)) iterator(oldDateMap.dropRight(1), newDateMap.dropRight(1), oldDateMap.last._1)
        //TODO: Case when initial value is last and next value is first (ex: 4th week of month sep and 1st week of month oct coincides)
        else result - 1
      }
      else result - 1
    }
    iterator(previousDateMap, nextDateMap, FREQUENCY_UPPER_LIMIT)
  }

  /**
   * @author          Vaibhav Agarwal
   * @define          Function to generate a generalized representation for date: Sequence of timeSet Identifier to its value
   * @param timestamp timestamp in Long
   * @return          the dateMap
   */
  def dateMapGenerator(timestamp: Long): Seq[(Int,Int)] = {
    val date = new DateTime(timestamp)
    val dateMap = mutable.LinkedHashMap[Int,Int]()
    dateMap(0) = date.millisOfSecond().get()
    dateMap(1) = date.secondOfMinute().get()
    dateMap(2) = date.minuteOfHour().get()
    dateMap(3) = date.hourOfDay().get()
    dateMap(4) = date.dayOfWeek().get()
    val month = date.monthOfYear().get()
    val firstWeekInMonth = new DateTime().withMonthOfYear(month).withDayOfMonth(1).getWeekOfWeekyear
    dateMap(5) = date.weekOfWeekyear().get() - firstWeekInMonth + 1
    dateMap(6) = month
    dateMap(7) = date.year().get()
    dateMap.toSeq
  }

  /**
   * @author            Vaibhav Agarwal
   * @define            Main function to identify minimum possible frequency between all date present in data
   * @param timestamps  List of all timestamp for a data set
   * @return            The minimum frequency in data
   */
  def frequencyIdentifier(timestamps: List[Long]) = {
    def iterator(timestampsList: List[Long], frequency: Int): Int ={
      if(!timestampsList.tail.isEmpty){
        val head = timestampsList.head
        val tail = timestampsList.tail
        val next = tail.head
        val frequencyOfFirstTwoDate = tailFrequencyComparator(dateMapGenerator(head), dateMapGenerator(next))
        if (frequencyOfFirstTwoDate < frequency) iterator(tail, frequencyOfFirstTwoDate) else iterator(tail,frequency)
      }
      else frequency
    }
    FREQUENCY_MAP(iterator(timestamps, FREQUENCY_UPPER_LIMIT))
  }
}
