package farrington.core.algorithm

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.SortedMap
import scala.io.Source
import org.json4s.native.JsonMethods.compact
import org.json4s.native.JsonMethods.parse
import org.json4s.native.JsonMethods.pretty
import org.json4s.native.JsonMethods.render
import org.json4s.string2JsonInput
import org.rosuda.REngine.Rserve.RConnection
import farrington.core.json.JSON
import farrington.core.result.Date
import farrington.core.result.Result
import farrington.core.result.ResultVector
import play.api.libs.json.Json

object Farrington {
  
  trait Mode {
    val rFlag: String
  }
  case object FarNew extends Mode{
    val rFlag = "farNew"
  }
  case object APHA extends Mode{
    val rFlag = "apha"
  }
  case object Stl extends Mode{
    val rFlag = "stl"
  }
  
  /*
   * TODO
   * 
   *  - Make more robust, returning a Try[Result] to gracefully handle problems in R
   *  - An option to save/retrieve intermediate JSON to help with debugging of R
   *    (could put something in the RServeHelper)
   *  - Embed R script using """?
   * 
   */
  
  val cl = getClass.getClassLoader  
  
  def runFarrington(dataIn: SortedMap[Date, Int], rCon: RConnection, mode: Mode = APHA): Result = {
    
    val rScript = Source.fromURI(cl.getResource("farrington/script.r").toURI()).mkString
    
    import JSON.timeSeriesWrites
    val json = Json.toJson(dataIn)
        
    //Debug
    JSON.writeJSON(json, Paths.get("outR.json"))
    
    // Farrington algorithm:
    val rExpression = {      
      import rCon._
      parseAndEval("""library(rjson)""")
      assign("jsonIn", Json.prettyPrint(json))
      assign("modeFlag", mode.rFlag)
      parseAndEval(rScript)
      parseAndEval("output")
    }      
    val rOut = Json.parse(rExpression.asString())   
    
    val (date, value) = dataIn.last  
    
    Result(date, value, rOut)
    
  }
  
  // New Farrington algorithm:
  def runFarringtonNew(dataIn: SortedMap[Date, Int], rCon: RConnection, mode: Mode = FarNew, nYearsBack: Int = 5): ResultVector = {
    
      val rScript = Source.fromURI(cl.getResource("farrington/scriptFarNew.r").toURI()).mkString  
    
      import JSON.timeSeriesWrites
      val json = Json.toJson(dataIn)
      
      //Debug
      JSON.writeJSON(json, Paths.get("outR.json"))
      
      val rExpression = {      
        import rCon._
        parseAndEval("""library(rjson)""")
        assign("jsonIn", Json.prettyPrint(json))
        assign("modeFlag", mode.rFlag)
        parseAndEval(rScript)
        parseAndEval("output")
      }        
      val rOut = Json.parse(rExpression.asString())
  
      val date = dataIn.keys.toIndexedSeq
      val value = dataIn.values.toIndexedSeq
      
      // TODO Check this is dropping from the right side and  add comment
      
      val resultvec = ResultVector(date, value, rOut)        
      val nDrop = date.length - resultvec.threshold.length
      
      ResultVector(date.drop(nDrop), value.drop(nDrop), rOut)
    
  }

}