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

  implicit val rnd = new scala.util.Random(42)


  "Loading MTBDataService instance" should "have succeeded" in { 

    serviceTry.isSuccess mustBe true

  }


  val service = serviceTry.get


  "Importing MTBFile" should "succeed" in {
    
    for {
      response <- service ! Upload(Gen.of[MTBFile].next)
      ok       <- response.isRight mustBe true
    } yield ok

  }


  "Importing MTBFile" should "return detected Issues" in {
   
    val mtbfile = Gen.of[MTBFile].next.invalidate
    val patId = mtbfile.patient.id

    for {

      response <- service ! Upload(mtbfile)

      Imported(result,_) = response.toOption.value 

      issuesDetected = result.isLeft mustBe true

      Left(qcReport) = result

      _ = qcReport.patient mustBe patId

      patients <- service.patientsWithIncompleteData 

      _ = patients must not be empty

      fetchedQCReport <- service.dataQualityReport(patId)

      _ = fetchedQCReport.value mustBe qcReport

    } yield issuesDetected

  }



}
