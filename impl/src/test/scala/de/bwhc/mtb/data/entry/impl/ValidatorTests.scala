package de.bwhc.mtb.data.entry.impl


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._

import cats.data.ValidatedNel

import de.ekut.tbi.generators.Gen

import de.bwhc.mtb.data.entry.dtos._
import de.bwhc.mtb.data.gens._
import de.bwhc.util.data.Validation.Validator
import de.bwhc.util.data.Validation.validate

import de.bwhc.mtb.data.entry.api.DataQualityReport.Issue
import de.bwhc.mtb.data.entry.api.DataQualityReport.Issue.Severity._


class ValidatorTests extends AnyFlatSpec 
{

  import DefaultDataValidator._

  implicit val rnd = new scala.util.Random(42)


  private def validationIssuesOf[A](a: A)(implicit v: Validator[Issue,A]) =
    v(a).swap.toOption.value.toList.map(_.severity)



  val patient = Gen.of[Patient].next

  val specimen = genSpecimenFor(patient).next

  val histologyReports = List(genHistologyReportFor(specimen).next)

  implicit val patientId  = patient.id
  implicit val specimenId = List(specimen.id)
  implicit val histologyReportIds = histologyReports.map(_.id)

  val diagnosis = genDiagnosisFor(specimen,histologyReports).next

  implicit val diagnosisIds = List(diagnosis.id)
  implicit val icd10Codes   = diagnosis.icd10.toList.map(_.code)


  val previousGLTherapy = genPreviousGLTherapyFor(diagnosis).next

  val lastGLTherapy = genLastGLTherapyFor(diagnosis).next

  implicit val therapyIds = List(previousGLTherapy.id, lastGLTherapy.id)

  val tumorCellContent = genTumorCellContentFor(specimen, TumorCellContent.Method.Bioinformatic).next


  // **************************************************************************
  // NOTE: the following classes / data blocks are trivial and left out from test:
  // Consent
  // MTBEpisode
  // ECOGStatus
  // MolecularPathologyFinding
  // **************************************************************************



  "Patient validation" must "work as expected" in {

    validate(patient).isValid mustBe true

    validationIssuesOf(patient.copy(birthDate = None) ) must contain (Error)

    validationIssuesOf(patient.copy(insurance = None) ) must contain (Warning)

  }


  "Diagnosis validation" must "work as expected" in {

    validate(diagnosis).isValid mustBe true

    validationIssuesOf(diagnosis.copy(recordedOn = None)) must contain (Warning)

    validationIssuesOf(diagnosis.copy(guidelineTreatmentStatus = None)) must contain (Warning)

    validationIssuesOf(diagnosis.copy(icd10 = Some(Coding(ICD10GM("wrong"),None)))) must contain (Error)

  }


  "PreviousGLTherapy validation" must "work as expected" in {

    validate(previousGLTherapy).isValid mustBe true

    validationIssuesOf(previousGLTherapy.copy(therapyLine = None)) must contain (Warning)

//    validationIssuesOf(previousGLTherapy.copy(medication = Some(List(Coding(Medication("wrong"),None))))) must contain (Error)
    validationIssuesOf(previousGLTherapy.copy(medication = Some(List(Medication.Coding(Medication.Code("wrong"),Medication.System.ATC))))) must contain (Error)

  }


  "LastGLTherapy validation" must "work as expected" in {

    validate(lastGLTherapy).isValid mustBe true
    
    validationIssuesOf(lastGLTherapy.copy(therapyLine = None)) must contain (Warning)

//    validationIssuesOf(lastGLTherapy.copy(medication = Some(List(Coding(Medication("wrong"),None))))) must contain (Error)
    validationIssuesOf(lastGLTherapy.copy(medication = Some(List(Medication.Coding(Medication.Code("wrong"),Medication.System.ATC))))) must contain (Error)

    validationIssuesOf(lastGLTherapy.copy(reasonStopped = None)) must contain (Warning)

    validationIssuesOf(lastGLTherapy.copy(period = lastGLTherapy.period.map(_.copy(end = None)))) must contain (Warning)

  }


  "Specimen validation" must "work as expected" in {

    validate(specimen).isValid mustBe true
    
    validationIssuesOf(specimen.copy(`type`= None)) must contain (Warning)

    validationIssuesOf(specimen.copy(collection = None)) must contain (Warning)

  }


  "TumorCellContent validation" must "work as expected" in {

    validate(tumorCellContent).isValid mustBe true

    validationIssuesOf(tumorCellContent.copy(value = 1.5)) must contain (Error)

  }


  "HistologyReport and TumorMorphology validation" must "work as expected" in {

    val histologyReport = histologyReports.head

    validate(histologyReport).isValid mustBe true

    validationIssuesOf(histologyReport.copy(issuedOn = None)) must contain (Warning)
   
   
    val morphology = histologyReport.tumorMorphology.get
 
    validationIssuesOf(morphology.copy(value = morphology.value.copy(code = ICDO3M("wrong")))) must contain (Error)

  }





}
