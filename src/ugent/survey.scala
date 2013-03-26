package ugent

import scala.collection.mutable
import scala.io.Source
import Ordering.Implicits._
import Numeric.Implicits._

abstract class Table {

  /*Represents a table as a list of objects*/

  var records: List[mutable.Map[String, Double]] = List()

  val filename: String
  val fields: List[(String, Int, Int)]

  def readRecords {
    readFile(filename, fields)
    recode
  }

  def readFile(filename: String, fields: List[(String, Int, Int)]) {
    /*Reads a data file, builds one map per record.

        Args:
            filename: string name of the file to read

            fields: sequence of (name, start, end, case) tuples specifying 
            the fields to extract
        */
    val fp = Source.fromFile(filename).getLines.filter(!_.isEmpty)
    for (line <- fp) {
      val record = makeRecord(line, fields)
      addRecord(record)
    }
  }

  def makeRecord(line: String, fields: List[(String, Int, Int)]): mutable.Map[String, Double] = {
    /*Scans a line and returns a map with the appropriate fields.

        Args:
            line: string line from a data file

            fields: sequence of (name, start, end, cast) tuples specifying 
            the fields to extract

        Returns:
            Record with appropriate fields.
        */
    mutable.Map((for ((field, start, end) <- fields) yield (field, {
      val s = line.slice(start - 1, end).trim
      try {
        s.toDouble

      } catch {
        case e: java.lang.NumberFormatException => Double.NaN
      }
    })).toMap.toSeq: _*)
  }

  def addRecord(record: mutable.Map[String, Double]) =
    /*Adds a record to this table.

        Args:
            record: an object of one of the record types.
        */
    records = records :+ record

  def recode
  /*Child classes can override this to recode values.*/

}

class Respondents extends Table {
  /* Represents the respondent table. */

  val filename = "D:/statcat/2002FemResp.dat"

  val fields =
    /*Returns a tuple specifying the fields to extract.

        The elements of the tuple are field, start, end, case.

                field is the name of the variable
                start and end are the indices as specified in the NSFG docs
                cast is a case class int, float, etc.
        */
    List(("caseid", 1, 12))

  def recode = {}
}

class Pregnancies extends Table {
  /* Contains survey data about a Pregnancy. */

  val filename = "D:/statcat/2002FemPreg.dat"

  val fields =
    /*Gets information about the fields to extract from the survey data.

        Documentation of the fields for Cycle 6 is at
        http://nsfg.icpsr.umich.edu/cocoon/WebDocs/NSFG/public/index.htm

        Returns:
            sequence of (name, start, end, type) tuples
        */
    List(
      ("caseid", 1, 12),
      ("nbrnaliv", 22, 22),
      ("babysex", 56, 56),
      ("birthwgt_lb", 57, 58),
      ("birthwgt_oz", 59, 60),
      ("prglength", 275, 276),
      ("outcome", 277, 277),
      ("birthord", 278, 279),
      ("agepreg", 284, 287),
      ("finalwgt", 423, 440))

  def recode {
    for (rec <- records) {

      // divide mother's age by 100

      if (rec("agepreg") != Double.NaN)
        rec("agepreg") /= 100.0

      // convert weight at birth from lbs/oz to total ounces
      // note: there are some very low birthweights
      // that are almost certainly errors, but for now I am not
      // filtering
      if (rec("birthwgt_lb") != Double.NaN && rec("birthwgt_lb") < 20 &&
        rec("birthwgt_oz") != Double.NaN && rec("birthwgt_oz") <= 16)
        rec("totalwgt_oz") = rec("birthwgt_lb") * 16 + rec("birthwgt_oz")
      else
        rec("totalwgt_oz") = Double.NaN
    }
  }
}

object survey {

  def main(args: Array[String]) {
    val resp = new Respondents
    resp.readRecords
    println("Number of respondents " + resp.records.size)

    val preg = new Pregnancies
    preg.readRecords
    println("Number of pregnancies " + preg.records.size)
    
    
  }

}