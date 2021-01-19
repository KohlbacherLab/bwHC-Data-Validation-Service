package de.bwhc.mtb.data.entry.views


import java.time.{
  Instant,
  LocalDate,
  LocalDateTime
}
import java.time.temporal.Temporal
import java.time.format.DateTimeFormatter.{
  ISO_LOCAL_DATE,
  ISO_LOCAL_DATE_TIME,
  ISO_INSTANT
}

import de.bwhc.mtb.data.entry.dtos._

import de.bwhc.catalogs.icd
import de.bwhc.catalogs.icd._
import de.bwhc.catalogs.hgnc.{HGNCGene,HGNCCatalog}
import de.bwhc.catalogs.med
import de.bwhc.catalogs.med.MedicationCatalog


object mappings
{

  import ValueSets._

  import syntax._


  val icd10gmCatalog = ICD10GMCatalogs.getInstance.get

  val icdO3Catalog   = ICDO3Catalogs.getInstance.get

  val medicationCatalog = MedicationCatalog.getInstance.get


  implicit class TemporalOps[T <: Temporal](val t: T) extends AnyVal 
  {
     def format: String = {
       t match {
         case ld:  LocalDate     => ISO_LOCAL_DATE.format(ld)
         case ldt: LocalDateTime => ISO_LOCAL_DATE_TIME.format(ldt)
         case t:   Instant       => ISO_INSTANT.format(t)
       }
     }

  }



  implicit val patientToView: Patient => PatientView = {
    pat =>
      PatientView(
        pat.id,
        ValueSet[Gender.Value].getCode(pat.gender).map(_.display).get,
        pat.birthDate.toRight("N/A"),
        pat.managingZPM.toRight("-"),
        pat.insurance.toRight("-"),
        pat.dateOfDeath.toRight("-")
      )

  }


  implicit def periodToDisplay[T <: Temporal]: Period[T] => PeriodDisplay[T] = {

    case OpenEndPeriod(start,end) =>
      PeriodDisplay(s"${start.format} - ${end.map(_.format).getOrElse("N/A")}")

    case ClosedPeriod(start,end) =>
      PeriodDisplay(s"${start.format} - ${end.format}")

  }


  implicit val icd10ToOptDisplay: Coding[ICD10GM] => Option[ICD10Display] = {
     icd10 =>
       icd10gmCatalog.code(icd.ICD10GM.Code(icd10.code.value),icd.ICD10GM.Version(icd10.version.get))
         .map(c => s"${c.code.value} - ${c.display.get}")
         .map(ICD10Display(_))
  }

  implicit val icd10ToDisplay: Coding[ICD10GM] => ICD10Display = {
     icd10 =>
       ICD10Display(s"${icd10.code.value} - ${icd10.display.getOrElse("N/A")}")
  }


  implicit val icdO3TtoOptDisplay: Coding[ICDO3T] => Option[ICDO3TDisplay] = {
     icdO3T =>
       icdO3Catalog.topographyCodings()
         .find(_.code == icd.ICDO3.TopographyCode(icdO3T.code.value))
         .map(c => s"${c.code.value} - ${c.display.get}")
         .map(ICDO3TDisplay(_))
  }

  implicit val icdO3TtoDisplay: Coding[ICDO3T] => ICDO3TDisplay = {
     icdO3T =>
       ICDO3TDisplay(s"${icdO3T.code.value} - ${icdO3T.display.getOrElse("N/A")}")
  }


  implicit val icdO3MtoOptDisplay: Coding[ICDO3M] => Option[ICDO3MDisplay] = {
     icdO3M =>
       icdO3Catalog.morphologyCodings()
         .find(_.code == icd.ICDO3.MorphologyCode(icdO3M.code.value))
         .map(c => s"${c.code.value} - ${c.display.get}")
         .map(ICDO3MDisplay(_))
  }

  implicit val icdO3MtoDisplay: Coding[ICDO3M] => ICDO3MDisplay = {
     icdO3M =>
       ICDO3MDisplay(s"${icdO3M.code.value} - ${icdO3M.display.getOrElse("N/A")}")
  }



  implicit val medicationToOptDisplay: Coding[Medication] => Option[MedicationDisplay] = {
    c =>
      medicationCatalog.findByCode(med.Medication.Code(c.code.value))
//        .flatMap(_.name)
        .map(c => s"${c.code.value} - ${c.name.get}")
        .map(MedicationDisplay(_))
  }


  implicit val medicationToDisplay: Coding[Medication] => MedicationDisplay = {
    c => MedicationDisplay(s"${c.code.value} - ${c.display.getOrElse("N/A")}")
  }


  implicit val diagnosisToView: Diagnosis => DiagnosisView = {
    diag => 

      DiagnosisView(
        diag.id,
        diag.patient,
        diag.recordedOn.toRight("N/A"),
        diag.icd10.flatMap(_.toOpt[ICD10Display]).getOrElse(Default.valueOf[ICD10Display]),
        diag.icdO3T.flatMap(_.toOpt[ICDO3TDisplay]).getOrElse(Default.valueOf[ICDO3TDisplay]),
        diag.whoGrade
          .flatMap(c => ValueSet[WHOGrade.Value].getCode(c.code))
          .map(c => s"${c.code} - ${c.display}")
          .getOrElse("-"),
        diag.guidelineTreatmentStatus
          .flatMap(c => ValueSet[GuidelineTreatmentStatus.Value].getCode(c))
          .map(_.display)
          .getOrElse("-")

      )

  }


  implicit val famMemDiagnosisToView: FamilyMemberDiagnosis => FamilyMemberDiagnosisView = {
    fmdiag =>
      FamilyMemberDiagnosisView(
        fmdiag.id,
        ValueSet[FamilyMember.Relationship.Value].getCode(fmdiag.relationship.code).get.display
      )
  }


  implicit val prevGLTherapyToView: PreviousGuidelineTherapy => PreviousGuidelineTherapyView = {
    th =>
      PreviousGuidelineTherapyView(
        th.id,
        th.patient,
        th.diagnosis,
        th.therapyLine.toRight("N/A"),
        th.medication.map(_.to[MedicationDisplay]),
      )

  }


  implicit val lastGLTherapyToView: LastGuidelineTherapy => LastGuidelineTherapyView = {
    th =>
      LastGuidelineTherapyView(
        th.id,
        th.patient,
        th.diagnosis,
        th.therapyLine.toRight("N/A"),
        th.period.map(_.asInstanceOf[Period[LocalDate]].to[PeriodDisplay[LocalDate]]).toRight("N/A"),
        th.medication.map(_.to[MedicationDisplay]),
        th.reasonStopped.flatMap(c => ValueSet[GuidelineTherapy.StopReason.Value].getCode(c.code))
          .map(_.display)
          .getOrElse("N/A")
      )

  }



  implicit val ecogToDisplay: ECOG.Value => ECOGDisplay = {
    ecog =>
      ValueSet[ECOG.Value]
        .getCode(ecog)
        .map(_.display) 
        .map(ECOGDisplay(_))
        .get  // safe to call 
  }



  implicit val specimenToView: Specimen => SpecimenView = {
    specimen =>
      SpecimenView(
        specimen.id,
//        specimen.patient,
        specimen.icd10.to[ICD10Display],
        specimen.`type`.flatMap(ValueSet[Specimen.Type.Value].getCode(_))
          .map(_.display)
          .getOrElse("N/A"),
        specimen.collection.map(_.date).toRight("N/A"),
        specimen.collection.map(_.localization)
          .flatMap(ValueSet[Specimen.Collection.Localization.Value].getCode(_))
          .map(_.display)
          .getOrElse("N/A"),
        specimen.collection.map(_.method)
          .flatMap(ValueSet[Specimen.Collection.Method.Value].getCode(_))
          .map(_.display)
          .getOrElse("N/A"),
      )
  }


  implicit val molPathoToView: MolecularPathologyFinding => MolecularPathologyFindingView = {
    finding =>
      MolecularPathologyFindingView(
        finding.id,
        finding.specimen,
        finding.performingInstitute.toRight("N/A"),
        finding.issuedOn.toRight("N/A"),
        finding.note
      )
  }


  implicit val histologyReportToView: HistologyReport => HistologyReportView = {
    report =>
      HistologyReportView(
        report.id,
        report.specimen,
        report.issuedOn.toRight("N/A"),
        report.tumorMorphology.map(_.value.to[ICDO3MDisplay]).toRight("N/A"),
        report.tumorCellContent.toRight("N/A"),
        report.tumorMorphology.flatMap(_.note).getOrElse("-"),
      )
  }





  implicit val mtbFileToView: MTBFile => MTBFileView = {
    mtbfile =>

      MTBFileView(
        mtbfile.patient.to[PatientView],
        mtbfile.diagnoses.getOrElse(List.empty[Diagnosis]).map(_.to[DiagnosisView]),
        mtbfile.familyMemberDiagnoses.getOrElse(List.empty[FamilyMemberDiagnosis]).map(_.to[FamilyMemberDiagnosisView]),
        mtbfile.previousGuidelineTherapies.getOrElse(List.empty[PreviousGuidelineTherapy]).map(_.to[PreviousGuidelineTherapyView]),
        mtbfile.lastGuidelineTherapy.map(_.to[LastGuidelineTherapyView]),
        mtbfile.specimens.getOrElse(List.empty[Specimen]).map(_.to[SpecimenView]),
        mtbfile.molecularPathologyFindings.getOrElse(List.empty[MolecularPathologyFinding]).map(_.to[MolecularPathologyFindingView]),
        mtbfile.histologyReports.getOrElse(List.empty[HistologyReport]).map(_.to[HistologyReportView]),
      )

  }





}
