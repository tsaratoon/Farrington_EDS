package farrington.study

import java.nio.file.Paths
import sampler.r.rserve.RServeHelper
import farrington.core.simulate.SimulateOutbreakData
import farrington.core.algorithm.Farrington
import farrington.core.algorithm.EDS
import sampler.r.script.RScript
import farrington.core.script.CreateRScript
import java.nio.file.Files
import java.nio.charset.Charset
import farrington.core.measures.AverageMeasures
import farrington.core.measures.Measures
import org.rosuda.REngine.Rserve.RConnection
import farrington.core.outbreak.OutbreakData

object CompareEDS extends App {
  
  //=======================
  // User-defined parameters
  
  // Number of sets of data to simulate
  val nSimulations = 150
  
  // Number of months for which to simulate data:
  val nData = 462
  val endYear = 2014 
  
  // Choose "short" or "long" outbreaks
  // outbreakLength = "short"
  val outbreakLength = "long"
  
  // Choose log-Normal or epidemic curve outbreak
  // val outbreakShape = "logNormal"
  val outbreakShape = "epidemicCurve"
  
  // Define end of each period
  //Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
  val endBaseline = 146
  val endPreOutbreak = 182
  val endOutbreak = 282
  
  val magnitude = 0.6
  
  // Identifiers for results files
  val csv_Stats = "compareStats.csv" // CSV file to store simulated data from Scala
  val scriptName_Stats = "compareStats.r" // R script to import the CSV and plot the data
  
  // CSV file to store time to detection data
  val csv_APHA = "ttd_apha.csv"
  val csv_FarNew = "ttd_farNew.csv"
  val csv_Stl = "ttd_stl.csv"
  
  // R script to import the CSV and plot the data
  val script_APHA = "ttd_apha.r"
  val script_FarNew = "ttd_farNew.r"
  val script_Stl = "ttd_stl.r"
  
  // PDFs containing the plots
  val pdf_APHA = "ttd_apha.pdf"
  val pdf_FarNew = "ttd_farNew.pdf"
  val pdf_Stl = "ttd_stl.pdf"
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "compareFarrington")
  
  //=======================
  // Simulation
  
  RServeHelper.ensureRunning()

  val measures_apha = (0 until nSimulations).par.map{ i =>
    println(i)
    val simData = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
    val outbreakData = OutbreakData(simData.year, simData.month, simData.counts)  
    val EDS_APHA = EDS.run(outbreakData, endBaseline, Farrington.APHA)
    Measures.allMeasures(EDS_APHA, simData.start, simData.end)
  }.toIndexedSeq
  
  val measures_farNew = (0 until nSimulations).par.map{ i =>
    println(i)    
    val simData = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
    val outbreakData = OutbreakData(simData.year, simData.month, simData.counts)
    val EDS_FarNew = EDS.run(outbreakData, endBaseline, Farrington.FarNew)
    Measures.allMeasures(EDS_FarNew, simData.start, simData.end)
  }.toIndexedSeq
  
  val measures_stl = (0 until nSimulations).par.map{ i =>
    println(i)    
    val simData = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
    val outbreakData = OutbreakData(simData.year, simData.month, simData.counts)  
    val EDS_Stl = EDS.run(outbreakData, endBaseline, Farrington.Stl)  
    Measures.allMeasures(EDS_Stl, simData.start, simData.end)
  }.toIndexedSeq
  
  RServeHelper.shutdown
  
  val avgMeasures_apha = AverageMeasures.calculate(measures_apha, nSimulations)
  val avgMeasures_farNew = AverageMeasures.calculate(measures_farNew, nSimulations) 
  val avgMeasures_stl = AverageMeasures.calculate(measures_stl, nSimulations) 
  
  //=======================
  // Output and plot: Sensitivity and specificity measures  
      
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  // Write times to detection to CSV file for APHA
  val writerStats = Files.newBufferedWriter(resultsDir.resolve(csv_Stats), Charset.defaultCharset())
  writerStats.write("mode, pod, pocd, fpr, fprc, ppv, ppvc, ttd, ttcd, potd")
  writerStats.newLine
  writerStats.write("APHA," + AverageMeasures.toString(avgMeasures_apha))
  writerStats.newLine
  writerStats.write("FarringtonNew," + AverageMeasures.toString(avgMeasures_farNew))
  writerStats.newLine
  writerStats.write("Stl," + AverageMeasures.toString(avgMeasures_stl))
  writerStats.newLine
  writerStats.close
  
  // Write R script which imports and outputs table (html)
  val rScript_stats = CreateRScript.statsToTable(csv_Stats)
  
  // Run the script in R and save the resulting PDF in the results directory
  RScript.apply(rScript_stats, resultsDir.resolve(scriptName_Stats))
  
  //=======================
  // Output and plot: Time to detection
  
  // Create histogram data for time to detection in form of List(time, count)
  val TTD_APHA =
    measures_apha
      .map{i => if (i.TTD.length == 0) -1 else i.TTD.head}
      .groupBy{ x => x }.mapValues(_.size).toList.sorted
  val TTD_FarNew = 
    measures_farNew
      .map(i => if (i.TTD.length == 0) -1 else i.TTD.head)
      .groupBy{ x => x }.mapValues(_.size).toList.sorted
  val TTD_Stl =
    measures_stl
      .map(i => if (i.TTD.length == 0) -1 else i.TTD.head)
      .groupBy{ x => x }.mapValues(_.size).toList.sorted
  
  // Write times to detection to CSV file for each mode
  Measures.writeTTD(TTD_APHA, resultsDir, csv_APHA)
  Measures.writeTTD(TTD_FarNew, resultsDir, csv_FarNew)
  Measures.writeTTD(TTD_Stl, resultsDir, csv_Stl)
  
  // Write R script which imports and plots data in a pdf
  val rScript_apha = CreateRScript.plotTTD(csv_APHA, pdf_APHA)
  val rScript_farNew = CreateRScript.plotTTD(csv_FarNew, pdf_FarNew)
  val rScript_stl = CreateRScript.plotTTD(csv_Stl, pdf_Stl)
  
  // Run the script in R and save the resulting PDF in the results directory
  RScript.apply(rScript_apha, resultsDir.resolve(script_APHA))
  RScript.apply(rScript_farNew, resultsDir.resolve(script_FarNew))
  RScript.apply(rScript_stl, resultsDir.resolve(script_Stl))
  
}