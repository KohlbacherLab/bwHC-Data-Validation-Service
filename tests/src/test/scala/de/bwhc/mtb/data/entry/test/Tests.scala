package de.bwhc.mtb.data.entry.test


import java.nio.file.Files.createTempDirectory

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._

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

  implicit val rnd = new scala.util.Random(42)


  "Loading MTBDataService instance" should "have succeeded" in { 

    serviceTry.isSuccess mustBe true

  }


  val service = serviceTry.get


  "Importing MTBFile" should "succeed" in {

    import MTBDataService.Command._
    
    for {
      resp <- service ! Upload(Gen.of[MTBFile].next)
      ok   <- resp.isRight mustBe true
    } yield ok

  }



}
