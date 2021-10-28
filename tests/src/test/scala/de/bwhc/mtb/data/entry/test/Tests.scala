package de.bwhc.mtb.data.entry.test


import java.nio.file.Files.createTempDirectory

import scala.util.Left
import scala.concurrent.Future

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._

import de.ekut.tbi.generators.Gen

import de.bwhc.mtb.data.entry.api._
import de.bwhc.mtb.data.entry.dtos.MTBFile
import de.bwhc.mtb.data.gens._


object Setup
{

  val tmpDir = createTempDirectory("bwHC_data_entry_test").toFile

  tmpDir.deleteOnExit

  System.setProperty("bwhc.zpm.site","Tübingen")
  System.setProperty("bwhc.data.entry.dir", tmpDir.getAbsolutePath)

  lazy val serviceTry = MTBDataService.getInstance

}


class Tests extends AsyncFlatSpec
{

  import Setup._
  import Invalidators._
  import MTBDataService.Command._
  import MTBDataService.Response._
  import MTBDataService.Error._

  implicit val rnd = new scala.util.Random(42)


  "Loading MTBDataService instance" should "have succeeded" in { 

    serviceTry.isSuccess mustBe true

  }


  val service = serviceTry.get


  "Importing valid MTBFile" should "succeed" in {
    
    for {
      response <- service ! Upload(Gen.of[MTBFile].next)
      ok       <- response.isRight mustBe true
    } yield ok

  }


  "Importing referentially inconsistent MTBFile" should "have failed with Fatal Issues" in {
   
    val invalidMTBFile = Gen.of[MTBFile].next.withFatalIssues
    val patId = invalidMTBFile.patient.id

    for {

      response <- service ! Upload(invalidMTBFile)

      InvalidData(qcReport) = response.swap.toOption.value

      _ = qcReport.patient mustBe patId

      ok = qcReport.issues.map(_.severity).toList must contain (DataQualityReport.Issue.Severity.Fatal)

    } yield ok

  }


  "Importing invalid MTBFile" should "have detected Error Issues" in {
   
    val invalidMTBFile = Gen.of[MTBFile].next.withErrors
    val patId = invalidMTBFile.patient.id

    for {

      response <- service ! Upload(invalidMTBFile)

      IssuesDetected(qcReport,_) = response.toOption.value

      _ = qcReport.patient mustBe patId

      _ = qcReport.issues.map(_.severity).toList must contain (DataQualityReport.Issue.Severity.Error)

      patients <- service.patientsWithIncompleteData 

      _ = patients must not be empty

      fetchedQCReport <- service.dataQualityReport(patId)

      ok = fetchedQCReport.value mustBe qcReport

    } yield ok

  }


  "Importing acceptable MTBFile (Warnings only)" should "have succeeded" in {
   
    val invalidMTBFile = Gen.of[MTBFile].next.withWarnings
    val patId = invalidMTBFile.patient.id

    for {

      response <- service ! Upload(invalidMTBFile)

      ok = response.toOption.value mustBe an [Imported]

    } yield ok

  }




}
