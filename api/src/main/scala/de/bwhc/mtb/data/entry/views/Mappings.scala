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


  val icd10gmCatalog: ICD10GMCatalogs

  val icdO3Catalog: ICDO3Catalogs

  val medicationCatalog: MedicationCatalog



  implicit class MappingOps[T](val t: T)
  {
    def mapTo[V](implicit f: T => V) = f(t)
  }


  implicit class TemporalFormattingOps[T <: Temporal](val t: T)
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
        pat.birthDate.toRight(NotAvailable),
        pat.managingZPM.toRight(NotAvailable),
        pat.insurance.toRight(NotAvailable),
        pat.dateOfDeath.toRight(NotAvailable)
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
       icd10gmCatalog.code(
         icd.ICD10GM.Code(icd10.code.value),
//         icd.ICD10GM.Version(icd10.version.get)
      )
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
        .map(c => MedicationDisplay(c.name.get))
        .getOrElse(MedicationDisplay(c.display.getOrElse("N/A")))
//        .map(c => MedicationDisplay(s"${c.code.value} - ${c.name.get}"))
//        .getOrElse(MedicationDisplay(s"${c.code.value} - ${c.display.getOrElse("N/A")}"))
  }


  implicit val diagnosisToView: Diagnosis => DiagnosisView = {
    diag => 

      DiagnosisView(
        diag.id,
        diag.patient,
        diag.recordedOn.toRight(NotAvailable),
        diag.icd10.map(_.mapTo[ICD10Display]).getOrElse(ICD10Display("N/A")),
        diag.icdO3T.map(_.mapTo[ICDO3TDisplay]).getOrElse(ICDO3TDisplay("N/A")),
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


//  implicit def guidelineTherapyToView[T <: GuidelineTherapy](
//    implicit responses: List[Response]
//  ): T => GuidelineTherapyView = {
  implicit def guidelineTherapyToView[T <: GuidelineTherapy]: ((T,Option[Response])) => GuidelineTherapyView = {

    case (therapy,response) =>

      therapy match { 
      
        case th: PreviousGuidelineTherapy =>
          GuidelineTherapyView(
            th.id,
            th.diagnosis,
            th.therapyLine.toRight(NotAvailable),
            NotAvailable.asLeft[PeriodDisplay[LocalDate]],
            th.medication.map(_.mapTo[MedicationDisplay]),
            NotAvailable.asLeft[String],
            response
              .map(_.mapTo[ResponseDisplay])
              .toRight(NotAvailable)
          )
        
        case th: LastGuidelineTherapy =>
          GuidelineTherapyView(
            th.id,
            th.diagnosis,
            th.therapyLine.toRight(NotAvailable),
            th.period.map(_.mapTo[PeriodDisplay[LocalDate]]).toRight(NotAvailable),
            th.medication.map(_.mapTo[MedicationDisplay]),
            th.reasonStopped.flatMap(c => ValueSet[GuidelineTherapy.StopReason.Value].concept(c.code))
              .map(_.display)
              .toRight(NotAvailable),
            response
              .map(_.mapTo[ResponseDisplay])
              .toRight(NotAvailable)
          )
      
      }

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
          .toRight(NotAvailable),
        specimen.collection.map(_.date).toRight(NotAvailable),
        specimen.collection.map(_.localization)
          .flatMap(ValueSet[Specimen.Collection.Localization.Value].concept(_))
          .map(_.display)
          .toRight(NotAvailable),
        specimen.collection.map(_.method)
          .flatMap(ValueSet[Specimen.Collection.Method.Value].concept(_))
          .map(_.display)
          .toRight(NotAvailable),
      )
  }


  implicit val molPathoToView: MolecularPathologyFinding => MolecularPathologyFindingView = {
    finding =>
      MolecularPathologyFindingView(
        finding.id,
        finding.specimen,
        finding.performingInstitute.toRight(NotAvailable),
        finding.issuedOn.toRight(NotAvailable),
        finding.note
      )
  }


  implicit val histologyReportToView: HistologyReport => HistologyReportView = {
    report =>
      HistologyReportView(
        report.id,
        report.specimen,
        report.issuedOn.toRight(NotAvailable),
        report.tumorMorphology.map(_.value.mapTo[ICDO3MDisplay]).toRight(NotAvailable),
        report.tumorCellContent.toRight(NotAvailable),
        report.tumorMorphology.flatMap(_.note).getOrElse("-"),
      )
  }



  implicit val levelOfEvidenceToDisplay: LevelOfEvidence => LevelOfEvidenceDisplay = {
    loe =>
      LevelOfEvidenceDisplay(
        s"${loe.grading.code}, Zusätze: ${loe.addendums.flatMap(_.map(_.code.toString).reduceOption((a1,a2) => s"$a1,$a2")).getOrElse("keine")}"
      )
  }



  implicit def recommendationToDisplay(
    implicit icd10: ICD10Display
  ): TherapyRecommendation => TherapyRecommendationView = {
    rec =>
      TherapyRecommendationView(
        rec.id,
        icd10,
        rec.medication.map(_.mapTo[MedicationDisplay]),
        rec.priority.toRight(NotAvailable),
        rec.levelOfEvidence.map(_.mapTo[LevelOfEvidenceDisplay]).toRight(NotAvailable),
        rec.supportingVariants.getOrElse(List.empty[Variant.Id])
      )
  }


  implicit val carePlanToDisplay:
    ((CarePlan, Diagnosis, List[TherapyRecommendation], Option[StudyInclusionRequest])) => CarePlanView = {

    case (carePlan,diagnosis,recommendations,studyInclusion) =>

      implicit val icd10 = diagnosis.icd10.map(_.mapTo[ICD10Display]).get

      CarePlanView(
        carePlan.id,
        icd10,
        carePlan.issuedOn.toRight(NotAvailable),
        carePlan.description.toRight(NotAvailable),
        carePlan.geneticCounsellingRequest.isDefined,
        studyInclusion.map(_.nctNumber).toRight(NotAvailable),
        carePlan.noTargetFinding.isDefined,
        recommendations.map(_.mapTo[TherapyRecommendationView]),
        carePlan.rebiopsyRequests.toRight(NotAvailable),
      )

  }


  implicit val carePlanDataToDisplay:
    (
      (List[CarePlan],
       List[Diagnosis],
       List[TherapyRecommendation],
       List[StudyInclusionRequest])
    ) => List[CarePlanView] = {

      case (carePlans,diagnoses,recommendations,studyInclusionReqs) =>

        carePlans.map {
          cp =>

//            val cpRecs = cp.recommendations.getOrElse(List.empty[TherapyRecommendation.Id])

            (
              cp,
              diagnoses.find(_.id == cp.diagnosis).get,
//              recommendations.filter(rec => cpRecs contains rec.id),
              cp.recommendations.fold(List.empty[TherapyRecommendation])(recs => recommendations.filter(rec => recs contains rec.id)),
              cp.studyInclusionRequest
                .flatMap(reqId => studyInclusionReqs.find(_.id == reqId))
            )
            .mapTo[CarePlanView]
        }

  }







  implicit val claimWithResponseToDisplay: ((Claim,Option[ClaimResponse])) => ClaimStatusView = {

    case (claim,response) =>

      ClaimStatusView(
        claim.id,
        claim.therapy,
        claim.issuedOn,
        response.map(_.issuedOn).toRight(NotAvailable),
        response.map(_.status)
          .flatMap(s => ValueSet[ClaimResponse.Status.Value].concept(s))
          .map(c => ClaimResponseStatusDisplay(c.display))
          .toRight(NotAvailable),
        response.flatMap(_.reason)
          .flatMap(s => ValueSet[ClaimResponse.Reason.Value].concept(s))
          .map(c => ClaimResponseReasonDisplay(c.display))
          .toRight(NotAvailable),
      )
  }



  implicit val molecularTherapyToView: ((MolecularTherapy,Option[Response])) => MolecularTherapyView = {

    case (molTh,resp) =>

    val status   = ValueSet[MolecularTherapy.Status.Value].concept(molTh.status).get.display
    val note     = molTh.note.getOrElse("-")
    val response = resp.map(_.mapTo[ResponseDisplay]).toRight(NotAvailable)

    molTh match { 
 
      case th: NotDoneTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          NotAvailable.asLeft[PeriodDisplay[LocalDate]],
          ValueSet[MolecularTherapy.NotDoneReason.Value].concept(th.notDoneReason.code).get.display,
          List.empty[MedicationDisplay],
          "-",
          NotAvailable.asLeft[Dosage.Value],
          note,
          response
        )
       
      case th: StoppedTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          "-",
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          ValueSet[MolecularTherapy.StopReason.Value].concept(th.reasonStopped.code).get.display,
          th.dosage.toRight(NotAvailable),
          note,
          response
        )
        
      case th: CompletedTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          "-",
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          "-",
          th.dosage.toRight(NotAvailable),
          note,
          response
        )
        
      case th: OngoingTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          "-",
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          "-",
          th.dosage.toRight(NotAvailable),
          note,
          response
        )

    }

  }








  implicit val mtbFileToView: MTBFile => MTBFileView = {

    mtbfile =>

      val responses = mtbfile.responses.getOrElse(List.empty[Response])

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
          .map(th => (th,responses.find(_.therapy == th.id)))
          .map(_.mapTo[GuidelineTherapyView]) ++
            mtbfile.lastGuidelineTherapy
              .map(th => (th,responses.find(_.therapy == th.id)))
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

        (
          mtbfile.carePlans.getOrElse(List.empty[CarePlan]),
          mtbfile.diagnoses.getOrElse(List.empty[Diagnosis]),
          mtbfile.recommendations.getOrElse(List.empty[TherapyRecommendation]),
          mtbfile.studyInclusionRequests.getOrElse(List.empty[StudyInclusionRequest])
        )
        .mapTo[List[CarePlanView]],

        mtbfile.claims
          .getOrElse(List.empty[Claim])
          .map(cl => (cl,claimResponses.find(_.claim == cl.id)))
          .map(_.mapTo[ClaimStatusView])
      )

  }




}

object mappings extends mappings
{

  val icd10gmCatalog = ICD10GMCatalogs.getInstance.get

  val icdO3Catalog   = ICDO3Catalogs.getInstance.get

  val medicationCatalog = MedicationCatalog.getInstance.get

}
