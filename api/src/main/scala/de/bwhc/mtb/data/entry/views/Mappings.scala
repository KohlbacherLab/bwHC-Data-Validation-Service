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


import cats.syntax.either._

import de.bwhc.mtb.data.entry.dtos._

import de.bwhc.catalogs.icd
import de.bwhc.catalogs.icd._
import de.bwhc.catalogs.hgnc.{HGNCGene,HGNCCatalog}
import de.bwhc.catalogs.med
import de.bwhc.catalogs.med.MedicationCatalog


//object mappings
trait mappings
{

  import ValueSets._

  import syntax._


  val icd10gmCatalog: ICD10GMCatalogs

  val icdO3Catalog: ICDO3Catalogs

  val medicationCatalog: MedicationCatalog

/*
  val icd10gmCatalog = ICD10GMCatalogs.getInstance.get

  val icdO3Catalog   = ICDO3Catalogs.getInstance.get

  val medicationCatalog = MedicationCatalog.getInstance.get
*/


  implicit class TemporalFormattingOps[T <: Temporal](val t: T)// extends AnyVal 
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
        ValueSet[Gender.Value].concept(pat.gender).map(_.display).get,
        pat.birthDate.toRight("N/A"),
        pat.managingZPM.toRight("-"),
        pat.insurance.toRight("-"),
        pat.dateOfDeath.toRight("-")
      )

  }


  implicit def periodToDisplay[T <: Temporal, P <: Period[T]]: P => PeriodDisplay[T] = {

    case OpenEndPeriod(start,end) =>
      PeriodDisplay(s"${start.format} - ${end.map(_.format).getOrElse("N/A")}")

    case ClosedPeriod(start,end) =>
      PeriodDisplay(s"${start.format} - ${end.format}")

  }



  implicit val icd10ToDisplay: Coding[ICD10GM] => ICD10Display = {
     icd10 =>
       icd10gmCatalog.code(icd.ICD10GM.Code(icd10.code.value),icd.ICD10GM.Version(icd10.version.get))
         .map(c => ICD10Display(s"${c.code.value} - ${c.display.get}"))
         .getOrElse(ICD10Display(s"${icd10.code.value} - ${icd10.display.getOrElse("N/A")}"))
  }


  implicit val icdO3TtoDisplay: Coding[ICDO3T] => ICDO3TDisplay = {
     icdO3T =>
      icdO3Catalog.topographyCodings()
        .find(_.code == icd.ICDO3.TopographyCode(icdO3T.code.value))
        .map(c => ICDO3TDisplay(s"${c.code.value} - ${c.display.get}"))
        .getOrElse(ICDO3TDisplay(s"${icdO3T.code.value} - ${icdO3T.display.getOrElse("N/A")}"))
  }


  implicit val icdO3MtoDisplay: Coding[ICDO3M] => ICDO3MDisplay = {
    icdO3M =>
      icdO3Catalog.morphologyCodings()
        .find(_.code == icd.ICDO3.MorphologyCode(icdO3M.code.value))
        .map(c => ICDO3MDisplay(s"${c.code.value} - ${c.display.get}"))
        .getOrElse(ICDO3MDisplay(s"${icdO3M.code.value} - ${icdO3M.display.getOrElse("N/A")}"))
  }


  implicit val medicationToDisplay: Coding[Medication] => MedicationDisplay = {
    c =>
      medicationCatalog
        .findByCode(med.Medication.Code(c.code.value))
        .map(c => MedicationDisplay(s"${c.code.value} - ${c.name.get}"))
        .getOrElse(MedicationDisplay(s"${c.code.value} - ${c.display.getOrElse("N/A")}"))
  }


  implicit val diagnosisToView: Diagnosis => DiagnosisView = {
    diag => 

      DiagnosisView(
        diag.id,
        diag.patient,
        diag.recordedOn.toRight("N/A"),
        diag.icd10.map(_.mapTo[ICD10Display]).getOrElse(Default.valueOf[ICD10Display]),
        diag.icdO3T.map(_.mapTo[ICDO3TDisplay]).getOrElse(Default.valueOf[ICDO3TDisplay]),
        diag.whoGrade
          .flatMap(c => ValueSet[WHOGrade.Value].concept(c.code))
          .map(c => s"${c.code} - ${c.display}")
          .getOrElse("-"),
        diag.guidelineTreatmentStatus
          .flatMap(c => ValueSet[GuidelineTreatmentStatus.Value].concept(c))
          .map(_.display)
          .getOrElse("-")

      )

  }


  implicit val famMemDiagnosisToView: FamilyMemberDiagnosis => FamilyMemberDiagnosisView = {
    fmdiag =>
      FamilyMemberDiagnosisView(
        fmdiag.id,
        ValueSet[FamilyMember.Relationship.Value]
          .concept(fmdiag.relationship.code)
          .get
          .display
      )
  }



  implicit val responseToDisplay: Response => ResponseDisplay = {
    resp =>
      ValueSet[RECIST.Value]
        .concept(resp.value.code)
        .map(c => ResponseDisplay(c.display))
        .get
  }


  implicit def guidelineTherapyToView[T <: GuidelineTherapy](
    implicit responses: List[Response]
  ): T => GuidelineTherapyView = {

    case th: PreviousGuidelineTherapy =>
      GuidelineTherapyView(
        th.id,
        th.diagnosis,
        th.therapyLine.toRight("N/A"),
        "N/A".asLeft[PeriodDisplay[LocalDate]],
        th.medication.map(_.mapTo[MedicationDisplay]),
        "N/A",
        responses.find(_.therapy == th.id)
          .map(_.mapTo[ResponseDisplay])
          .toRight("N/A")
      )

    case th: LastGuidelineTherapy =>
      GuidelineTherapyView(
        th.id,
        th.diagnosis,
        th.therapyLine.toRight("N/A"),
        th.period.map(_.mapTo[PeriodDisplay[LocalDate]]).toRight("N/A"),
        th.medication.map(_.mapTo[MedicationDisplay]),
        th.reasonStopped.flatMap(c => ValueSet[GuidelineTherapy.StopReason.Value].concept(c.code))
          .map(_.display)
          .getOrElse("N/A"),
        responses.find(_.therapy == th.id)
          .map(_.mapTo[ResponseDisplay])
          .toRight("N/A")
      )

  }



  implicit val ecogToDisplay: Coding[ECOG.Value] => ECOGDisplay = {
    ecog =>
      ValueSet[ECOG.Value]
        .concept(ecog.code)
        .map(_.display) 
        .map(ECOGDisplay(_))
        .get  // safe to call 
  }

  implicit val ecogsToDisplay: List[ECOGStatus] => ECOGStatusView = {
    ecogs =>
      ECOGStatusView(
        ecogs.map(ecog =>
          TemporalValue(
            ecog.effectiveDate.map(_.format(ISO_LOCAL_DATE)).getOrElse("N/A"),
            ecog.value.mapTo[ECOGDisplay]
          )
        )
      )
  }



  implicit val specimenToView: Specimen => SpecimenView = {
    specimen =>
      SpecimenView(
        specimen.id,
        specimen.icd10.mapTo[ICD10Display],
        specimen.`type`.flatMap(ValueSet[Specimen.Type.Value].concept(_))
          .map(_.display)
          .getOrElse("N/A"),
        specimen.collection.map(_.date).toRight("N/A"),
        specimen.collection.map(_.localization)
          .flatMap(ValueSet[Specimen.Collection.Localization.Value].concept(_))
          .map(_.display)
          .getOrElse("N/A"),
        specimen.collection.map(_.method)
          .flatMap(ValueSet[Specimen.Collection.Method.Value].concept(_))
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
        report.tumorMorphology.map(_.value.mapTo[ICDO3MDisplay]).toRight("N/A"),
        report.tumorCellContent.toRight("N/A"),
        report.tumorMorphology.flatMap(_.note).getOrElse("-"),
      )
  }



  implicit val claimWithResponseToDisplay: ((Claim,Option[ClaimResponse])) => ClaimStatusView = {

    case (claim,response) =>

      ClaimStatusView(
        claim.id,
        claim.therapy,
        claim.issuedOn,
        response.map(_.issuedOn).toRight("N/A"),
        response.map(_.status)
          .flatMap(s => ValueSet[ClaimResponse.Status.Value].concept(s))
          .map(c => ClaimResponseStatusDisplay(c.display))
          .toRight("N/A"),
        response.flatMap(_.reason)
          .flatMap(s => ValueSet[ClaimResponse.Reason.Value].concept(s))
          .map(c => ClaimResponseReasonDisplay(c.display))
          .toRight("N/A"),
      )
  }



//  implicit val molecularTherapyToView: MolecularTherapy => MolecularTherapyView = {
  implicit val molecularTherapyToView: ((MolecularTherapy,Option[Response])) => MolecularTherapyView = {

    case (molTh,resp) =>

    val status   = ValueSet[MolecularTherapy.Status.Value].concept(molTh.status).get.display
    val note     = molTh.note.getOrElse("-")
    val response = resp.map(_.mapTo[ResponseDisplay]).toRight("N/A")

    molTh match { 
 
      case th: NotDoneTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          "-".asLeft[PeriodDisplay[LocalDate]],
          ValueSet[MolecularTherapy.NotDoneReason.Value].concept(th.notDoneReason.code).get.display,
          List.empty[MedicationDisplay],
          "-",
          "-".asLeft[Dosage.Value],
          note,
          response
        )
       
      case th: StoppedTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[String],
          "-",
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          ValueSet[MolecularTherapy.StopReason.Value].concept(th.reasonStopped.code).get.display,
          th.dosage.toRight("N/A"),
          note,
          response
        )
        
      case th: CompletedTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[String],
          "-",
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          "-",
          th.dosage.toRight("N/A"),
          note,
          response
        )
        
      case th: OngoingTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[String],
          "-",
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          "-",
          th.dosage.toRight("N/A"),
          note,
          response
        )

    }

  }








  implicit val mtbFileToView: MTBFile => MTBFileView = {

    mtbfile =>

      implicit val responses = mtbfile.responses.getOrElse(List.empty[Response])

      val claimResponses = mtbfile.claimResponses.getOrElse(List.empty[ClaimResponse])


      MTBFileView(
        mtbfile.patient.mapTo[PatientView],

        mtbfile.diagnoses
          .getOrElse(List.empty[Diagnosis])
          .map(_.mapTo[DiagnosisView]),

        mtbfile.familyMemberDiagnoses
          .getOrElse(List.empty[FamilyMemberDiagnosis])
          .map(_.mapTo[FamilyMemberDiagnosisView]),

        mtbfile.previousGuidelineTherapies
          .getOrElse(List.empty[PreviousGuidelineTherapy])
          .map(_.mapTo[GuidelineTherapyView]) ++
            mtbfile.lastGuidelineTherapy
              .map(_.mapTo[GuidelineTherapyView]),

        mtbfile.ecogStatus.map(_.mapTo[ECOGStatusView]),

        mtbfile.specimens
          .getOrElse(List.empty[Specimen])
          .map(_.mapTo[SpecimenView]),

        mtbfile.molecularPathologyFindings
          .getOrElse(List.empty[MolecularPathologyFinding])
          .map(_.mapTo[MolecularPathologyFindingView]),

        mtbfile.histologyReports
          .getOrElse(List.empty[HistologyReport])
          .map(_.mapTo[HistologyReportView]),

        mtbfile.claims
          .getOrElse(List.empty[Claim])
          .map(cl => (cl,claimResponses.find(_.claim == cl.id)))
          .map(_.mapTo[ClaimStatusView])
      )

  }




}
