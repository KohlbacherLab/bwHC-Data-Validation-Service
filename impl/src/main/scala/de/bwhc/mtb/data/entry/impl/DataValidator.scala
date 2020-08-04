package de.bwhc.mtb.data.entry.impl


import java.time.LocalDate
import java.time.temporal.Temporal

import scala.util.Either
import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.NonEmptyList

import de.bwhc.util.spi._
import de.bwhc.util.data.Validation._

import de.bwhc.mtb.data.entry.dtos._
import de.bwhc.mtb.data.entry.api.DataQualityReport

import de.bwhc.catalogs.icd
import de.bwhc.catalogs.icd._
import de.bwhc.catalogs.med.MedicationCatalog



trait DataValidator
{

  def check(
    mtbfile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[Either[DataQualityReport,MTBFile]]

}

trait DataValidatorProvider extends SPI[DataValidator]

object DataValidator extends SPILoader(classOf[DataValidatorProvider])




object DefaultDataValidator extends DataValidator
{

  import DataQualityReport._
  import DataQualityReport.Issue._

  import cats.data.Validated
  import Validated._

  import cats.syntax.apply._
  import cats.syntax.traverse._
  import cats.syntax.validated._
  import cats.instances.list._
  import cats.instances.set._


  def check(
    mtbfile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[Either[DataQualityReport,MTBFile]] = {
    Future.successful(
      mtbfile.validate
        .leftMap(DataQualityReport(mtbfile.patient.id,_))
        .toEither
    )
  }


  type DataQualityValidator[T] = Validator[DataQualityReport.Issue,T]


  implicit val patientValidator: DataQualityValidator[Patient] = {

    case pat @ Patient(Patient.Id(id),_,birthDate,_,insurance,dod) =>

      (
        birthDate ifUndefined (Error("Missing BirthDate") at Location("Patient",id,"birthdate")),

        insurance ifUndefined (Warning("Missing Health Insurance") at Location("Patient",id,"insurance")),

        dod ifUndefined (Info("Undefined date of death. Ensure if up to date") at Location("Patient",id,"dateOfDeath"))
      )
      .mapN { case _: Product => pat}
  }


  implicit def consentValidator(
    implicit patId: Patient.Id
  ): DataQualityValidator[Consent] = {

    case consent @ Consent(id,patient,_) =>

      (patient mustBe patId otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("Consent",id.value,"patient")))
        .map(_ => consent)

  }


  implicit def episodeValidator(
    implicit patId: Patient.Id
  ): DataQualityValidator[MTBEpisode] = {
    case episode @ MTBEpisode(id,patient,period) =>

      (patient mustBe patId otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("MTBEpisode",id.value,"patient")))
        .map(_ => episode)

  }



  implicit lazy val icd10gm = ICD10GMCatalogs.getInstance.get

  implicit lazy val icdO3   = ICDO3Catalogs.getInstance.get


  implicit def icd10Validator(
    implicit
    catalog: ICD10GMCatalogs
  ): DataQualityValidator[Coding[ICD10GM]] = {

      case icd10 @ Coding(code,_,version) =>

        (version ifUndefined (Error("Missing ICD-10-GM Version") at Location("ICD-10-GM Coding","","version")))
          .andThen (
            v =>
              ifThrows(
                icd.ICD10GM.Version(v)
              )(
                Error(s"Invalid ICD-10-GM Version $version") at Location("ICD-10-GM Coding","","version")
              )
          )
          .andThen (
            v =>
              code.value mustBeIn catalog.codings(v).map(_.code.value)
                otherwise (Error(s"Invalid ICD-10-GM code $code") at Location("ICD-10-GM Coding","","code"))
          )
          .map(c => icd10)

    }

  implicit def icdO3TValidator(
    implicit
    catalog: ICDO3Catalogs
  ): DataQualityValidator[Coding[ICDO3T]] = {

      case icdo3t @ Coding(code,_,version) =>

        (version ifUndefined (Error("Missing ICD-O-3 Version") at Location("ICD-O-3-T Coding","","version")))
          .andThen(
            v =>
              ifThrows(icd.ICDO3.Version(v))(Error(s"Invalid ICD-O-3 Version $version") at Location("ICD-O-3-T Coding","","version"))
          )
          .andThen(
            v =>
              code.value mustBeIn catalog.topographyCodings(v).map(_.code.value)
                otherwise (Error(s"Invalid ICD-O-3-T code $code") at Location("ICD-O-3-T Coding","","code"))
          )
          .map(c => icdo3t)

    }

  implicit def icdO3MValidator(
    implicit
    catalog: ICDO3Catalogs
  ): DataQualityValidator[Coding[ICDO3M]] = {

      case icdo3m @ Coding(code,_,version) =>

        (version ifUndefined (Error("Missing ICD-O-3 Version") at Location("ICD-O-3-M Coding","","version")))
          .andThen(
            v =>
              ifThrows(icd.ICDO3.Version(v))(Error(s"Invalid ICD-O-3 Version $version") at Location("ICD-O-3-M Coding","","version"))
          )
          .andThen(
            v =>
              code.value mustBeIn catalog.morphologyCodings(v).map(_.code.value)
                otherwise (Error(s"Invalid ICD-O-3-M code $code") at Location("ICD-O-3-M Coding","","code"))
          )
          .map(c => icdo3m)

    }


  implicit val medicationCatalog = MedicationCatalog.getInstance.get

  implicit def medicationValidator(
    implicit
    catalog: MedicationCatalog
  ): DataQualityValidator[Coding[Medication]] = {

      case medication @ Coding(code,_,_) =>

        (code.value mustBeIn catalog.entries.map(_.code.value)
          otherwise (Error(s"Invalid ATC Medication code $code") at Location("Medication Coding","","code")))
         .map(c => medication)

    }


/*
final case class Diagnosis
(
  id: Diagnosis.Id,
  patient: Patient.Id,
  recordedOn: Option[LocalDate],
  icd10: Option[Coding[ICD10GM]],
  icdO3T: Option[Coding[ICDO3T]],
  whoGrade: Option[Coding[WHOGrade.Value]],
  histologyResults: Option[List[HistologyResult.Id]],
  statusHistory: Option[List[Diagnosis.StatusOnDate]]
)
*/

  implicit def diagnosisValidator(
    implicit
    patId: Patient.Id,
    specimenRefs: List[Specimen.Id],
    histologyRefs: List[HistologyResult.Id]
  ): DataQualityValidator[Diagnosis] = {

    case diag @ Diagnosis(Diagnosis.Id(id),patient,date,icd10,icdO3T,_,histologyResults,_) =>

      implicit val diagId = diag.id

      (
        (patient mustBe patId otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("Diagnosis",id,"patient"))),

        (date ifUndefined (Warning("Missing Recording Date") at Location("Diagnosis",id,"recordedOn"))),

        (icd10 ifUndefined (Error("Missing ICD-10-GM Coding") at Location("Diagnosis",id,"icd10")))
          andThen (_ validate),

        (icdO3T ifUndefined (Warning("Missing ICD-O-3-T Coding") at Location("Diagnosis",id,"icdO3T")))
          andThen (_ validate),

        histologyResults.map(
          refs => refs traverse (
            ref => ref mustBeIn histologyRefs
              otherwise (Error(s"Invalid Reference HistologyResult/${ref.value}") at Location("Diagnosis",id,"histologyResults"))
          )
        )
        .getOrElse(List.empty[HistologyResult.Id].validNel[Issue]) 
      )
      .mapN { case _: Product => diag}

  }


/*
case class PreviousGuidelineTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  therapyLine: Option[TherapyLine],
  medication: Option[Set[Coding[Medication]]],
) extends GuidelineTherapy
*/

  implicit val therapyLines = (0 to 9).map(TherapyLine(_))
  
  implicit def prevGuidelineTherapyValidator(
    implicit
    patId: Patient.Id,
    therapyLines: Seq[TherapyLine]
  ): DataQualityValidator[PreviousGuidelineTherapy] = {

    case th @ PreviousGuidelineTherapy(TherapyId(id),patient,therapyLine,medication) =>
      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("PreviousGuidelineTherapy",id,"patient"))),

        (therapyLine ifUndefined (Warning("Missing Therapy Line") at Location("PreviousGuidelineTherapy",id,"therapyLine")))
          andThen ( l =>
            l mustBeIn therapyLines otherwise (Error(s"Invalid Therapy Line ${l.value}") at Location("PreviousGuidelineTherapy",id,"therapyLine"))
          ),

        (medication ifUndefined (Warning("Missing Medication") at Location("PreviousGuidelineTherapy",id,"medication")))
          andThen (_ validateEach)
        
      )
      .mapN { case _: Product => th }
  }

/*
case class LastGuidelineTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  therapyLine: Option[TherapyLine],
  period: Option[OpenEndPeriod[LocalDate]],
  medication: Option[Set[Coding[Medication]]],
  reasonStopped: Option[GuidelineTherapy.StopReason.Value]
) extends GuidelineTherapy
*/

  implicit def lastGuidelineTherapyValidator(
    implicit
    patId: Patient.Id,
    therapyLines: Seq[TherapyLine],
    therapyRefs: Seq[TherapyId],
  ): DataQualityValidator[LastGuidelineTherapy] = {

    case th @ LastGuidelineTherapy(TherapyId(id),patient,therapyLine,period,medication,reasonStopped) =>
      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("LastGuidelineTherapy",id,"patient"))),

        (therapyLine ifUndefined (Warning("Missing Therapy Line") at Location("LastGuidelineTherapy",id,"therapyLine")))
          andThen ( l =>
            l mustBeIn therapyLines otherwise (Error(s"Invalid Therapy Line ${l.value}") at Location("LastGuidelineTherapy",id,"therapyLine"))
          ),
        
        (medication ifUndefined (Warning("Missing Medication") at Location("LastGuidelineTherapy",id,"medication"))
          andThen (_ validateEach)),

        (reasonStopped ifUndefined (Warning("Missing Stop Reason") at Location("LastGuidelineTherapy",id,"reasonStopped"))),

        (th.id mustBeIn therapyRefs
           otherwise (Warning("Missing Response") at Location("LastGuidelineTherapy",id,"response")))
      )
      .mapN { case _: Product => th }

  }

/*
case class ECOGStatus
(
  id: ECOGStatus.Id,
  patient: Patient.Id,
  effectiveDate: Option[LocalDate],
  value: Coding[ECOG.Value]
)
*/

  implicit def ecogStatusValidator(
    implicit
    patId: Patient.Id
  ): DataQualityValidator[ECOGStatus] = {

    case pfSt @ ECOGStatus(id,patient,_,_) =>

      (patient mustBe patId otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("ECOGStatus",id.value,"patient")))
        .map(_ => pfSt)

  }


/*
final case class Specimen
(
  id: Specimen.Id,
  patient: Patient.Id,
  icd10: Coding[ICD10GM],
  `type`: Option[Specimen.Type.Value],
  collection: Option[Specimen.Collection]
)
*/

  implicit def specimenValidator(
    implicit
    patId: Patient.Id,
    icd10codes: Seq[ICD10GM]
  ): DataQualityValidator[Specimen] = {

    case sp @ Specimen(Specimen.Id(id),patient,icd10,typ,collection) =>
      (
        (patient mustBe patId otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("Specimen",id,"patient"))),

        icd10.validate
          andThen (
            icd => icd.code mustBeIn icd10codes otherwise (Error(s"Invalid Reference to Diagnosis $icd") at Location("Specimen",id,"icd10"))
          ),
  
        (typ ifUndefined (Warning(s"Missing Specimen type") at Location("Specimen",id,"type"))),

        (collection ifUndefined (Warning(s"Missing Specimen collection") at Location("Specimen",id,"collection")))
       
      )
      .mapN { case _: Product => sp }

  }

/*
final case class HistologyResult
(
  id: HistologyResult.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  issuedOn: Option[LocalDate],
//  icd10: Option[Coding[ICD10GM]],
  icdO3M: Option[Coding[ICDO3M]],
  note: Option[String]
)
*/

  implicit def histologyValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id]
  ): DataQualityValidator[HistologyResult] = {

    case histo @ HistologyResult(HistologyResult.Id(id),patient,specimen,date,icdO3M,_) =>

      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("HistologyResult",id,"patient"))),

        (specimen mustBeIn specimens
           otherwise (Error(s"Invalid Reference to Specimen/${specimen.value}") at Location("HistologyResult",id,"specimen"))),

        (date ifUndefined (Error("Missing issue date") at Location("HistologyResult",id,"issuedOn"))),

        (icdO3M ifUndefined (Error("Missing ICD-O-3-M Coding") at Location("HistologyResult",id,"icdO3M")))
          andThen ( _ validate)

      )
      .mapN { case _: Product => histo }

  }


/*
case class CarePlan
(
  id: CarePlan.Id,
  patient: Patient.Id,
  issuedOn: LocalDate,
  description: Option[String],
  recommendations: NonEmptyList[TherapyRecommendation.Id],
  geneticCounselling: Option[Boolean],
  rebiopsyRequests: List[RebiopsyRequest.Id]
)
*/

  implicit def carePlanValidator(
    implicit
    patId: Patient.Id,
    recommendationRefs: Seq[TherapyRecommendation.Id],
    counsellingRequestRefs: Seq[GeneticCounsellingRequest.Id],
    rebiopsyRequestRefs: Seq[RebiopsyRequest.Id]
  ): DataQualityValidator[CarePlan] = {

    case cp @ CarePlan(CarePlan.Id(id),patient,date,_,recommendations,counsellingReq,rebiopsyRequests) =>

      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("CarePlan",id,"patient"))),

        (date ifUndefined (Warning("Missing Recording Date") at Location("CarePlan",id,"issuedOn"))),

        recommendations traverse (
          ref => ref mustBeIn recommendationRefs
            otherwise (Error(s"Invalid Reference to TherapyRecommendation/${ref.value}") at Location("CarePlan",id,"recommendations"))
        ),

        counsellingReq.map(
          ref => ref mustBeIn counsellingRequestRefs
           otherwise (Error(s"Invalid Reference to GeneticCounsellingRequest/${ref.value}") at Location("CarePlan",id,"geneticCounsellingRequest"))
        )
        .getOrElse(None.validNel[Issue]),
 
        rebiopsyRequests.map( refs =>
          refs traverse (
            ref => ref mustBeIn rebiopsyRequestRefs
              otherwise (Error(s"Invalid Reference to RebiopsyRequest/${ref.value}") at Location("CarePlan",id,"rebiopsyRequests"))
          )
        )
        .getOrElse(List.empty[RebiopsyRequest.Id].validNel[Issue]) 
      )
      .mapN { case _: Product => cp }

  }

/*
case class TherapyRecommendation
(
  id: TherapyRecommendation.Id,
  patient: Patient.Id,
  issuedOn: Option[LocalDate],
  medication: Set[Coding[Medication]],
  priority: Option[TherapyRecommendation.Priority.Value],
  levelOfEvidence: Option[LevelOfEvidence],
  supportingVariant: Option[Variant.CosmicId]
) 
*/
 
  implicit def recommendationValidator(
    implicit
    patId: Patient.Id
  ): DataQualityValidator[TherapyRecommendation] = {

    case rec @ TherapyRecommendation(TherapyRecommendation.Id(id),patient,date,medication,priority,loe,variant) =>

      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("TherapyRecommendation",id,"patient"))),

        (date ifUndefined (Warning("Missing Recording Date") at Location("TherapyRecommendation",id,"issuedOn"))),

        (medication validateEach),

        (priority ifUndefined (Warning("Missing Priority") at Location("TherapyRecommendation",id,"priority"))),

        (loe ifUndefined (Warning("Missing Level of Evidence") at Location("TherapyRecommendation",id,"levelOfEvidence"))),

        (variant ifUndefined (Warning("Missing supporting Variant") at Location("TherapyRecommendation",id,"supportingVariant"))),

      )
      .mapN { case _: Product => rec }

  }


  implicit def counsellingRequestValidator(
    implicit
    patId: Patient.Id
  ): DataQualityValidator[GeneticCounsellingRequest] = {

    case req @ GeneticCounsellingRequest(GeneticCounsellingRequest.Id(id),patient,date) =>

      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("GeneticCounsellingRequest",id,"patient"))),

        (date ifUndefined (Warning("Missing Recording Date") at Location("GeneticCounsellingRequest",id,"issuedOn"))),

      )
      .mapN { case _: Product => req }

  }

/*
final case class RebiopsyRequest
(
  id: RebiopsyRequest.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  issuedOn: Option[LocalDate]
)
*/

  implicit def rebiopsyRequestValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id]
  ): DataQualityValidator[RebiopsyRequest] = {

    case req @ RebiopsyRequest(RebiopsyRequest.Id(id),patient,specimen,date) =>

      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("RebiopsyRequest",id,"patient"))),

        (date ifUndefined (Warning("Missing Recording Date") at Location("RebiopsyRequest",id,"issuedOn"))),

        (specimen mustBeIn specimens
           otherwise (Error(s"Invalid Reference to Specimen/${specimen.value}") at Location("RebiopsyRequest",id,"specimen"))),
      )
      .mapN { case _: Product => req }

  }

/*
final case class Claim
(
  id: Claim.Id,
  patient: Patient.Id,
  issuedOn: LocalDate,
  therapy: TherapyRecommendation.Id
)
*/

  implicit def claimValidator(
    implicit
    patId: Patient.Id,
    recommendationRefs: Seq[TherapyRecommendation.Id],
  ): DataQualityValidator[Claim] = {

    case cl @ Claim(Claim.Id(id),patient,_,therapy) =>

      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("Claim",id,"patient"))),

        (therapy mustBeIn recommendationRefs
          otherwise (Error(s"Invalid Reference to TherapyRecommendation/${therapy.value}") at Location("Claim",id,"therapy")))

      )
      .mapN { case _: Product => cl }

  }

/*
final case class ClaimResponse
(
  id: ClaimResponse.Id,
  issuedOn: LocalDate,
  claim: Claim.Id,
  patient: Patient.Id,
  status: ClaimResponse.Status.Value,
  reason: Option[ClaimResponse.Reason.Value]
)
*/

  implicit def claimResponseValidator(
    implicit
    patId: Patient.Id,
    claimRefs: Seq[Claim.Id],
  ): DataQualityValidator[ClaimResponse] = {

    case cl @ ClaimResponse(ClaimResponse.Id(id),claim,patient,_,_,reason) =>

      (
        (patient mustBe patId
           otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("ClaimResponse",id,"patient"))),

        (claim mustBeIn claimRefs
          otherwise (Error(s"Invalid Reference to Claim/${claim.value}") at Location("ClaimResponse",id,"claim"))),

        (reason ifUndefined (Warning("Missing Reason for ClaimResponse Status") at Location("ClaimResponse",id,"reason")))
      )
      .mapN { case _: Product => cl }

  }



/*
final case class MTBFile
(
  patient: Patient,
  consent: Consent,
  episode: MTBEpisode,
  diagnoses: Option[List[Diagnosis]],
  familyMemberDiagnoses: Option[List[FamilyMemberDiagnosis]],
  previousGuidelineTherapies: Option[List[PreviousGuidelineTherapy]],
  lastGuidelineTherapy: Option[LastGuidelineTherapy],
  ecogStatus: Option[List[ECOGStatus]],
  specimens: Option[List[Specimen]],
  histologyResults: Option[List[HistologyResult]],
  //TODO: NGS reports
  carePlans: Option[List[CarePlan]],
  recommendations: Option[List[TherapyRecommendation]],
  rebiopsyRequests: Option[List[RebiopsyRequest]],
  claims: Option[List[Claim]],
  claimResponses: Option[List[ClaimResponse]],
  molecularTherapies: Option[List[MolecularTherapyDocumentation]],
  responses: Option[List[Response]]
)
*/

  implicit val mtbFileValidator: DataQualityValidator[MTBFile] = {

    case mtbfile @ MTBFile(
      patient,
      consent,
      episode,
      diagnoses,
      _,
      previousGuidelineTherapies,
      lastGuidelineTherapy,
      ecogStatus,
      specimens,
      histologyResults,
      carePlans,
      recommendations,
      counsellingRequests,
      rebiopsyRequests,
      claims,
      claimResponses,
      _,
      responses
    ) =>
    
      implicit val patId = patient.id  

      implicit val icd10codes =
        diagnoses.getOrElse(List.empty[Diagnosis])
          .map(_.icd10)
          .filter(_.isDefined)
          .map(_.get.code)

      implicit val histoRefs =
        histologyResults.getOrElse(List.empty[HistologyResult]).map(_.id)

      implicit val specimenRefs =
        specimens.getOrElse(List.empty[Specimen]).map(_.id)

      implicit val therapyRefs =
        responses.getOrElse(List.empty[Response]).map(_.therapy)

      implicit val recommendationRefs =
        recommendations.getOrElse(List.empty[TherapyRecommendation]).map(_.id)

      implicit val counsellingRequestRefs =
        counsellingRequests.getOrElse(List.empty[GeneticCounsellingRequest]).map(_.id)

      implicit val rebiopsyRequestRefs =
        rebiopsyRequests.getOrElse(List.empty[RebiopsyRequest]).map(_.id)

      implicit val claimRefs =
        claims.getOrElse(List.empty[Claim]).map(_.id)

      (
        patient.validate,

        consent.validate,

        episode.validate,

        (diagnoses ifUndefined (Error("Missing diagnosis records") at Location("MTBFile",patId.value,"diagnoses")))
          andThen (_ ifEmpty (Error("Missing diagnoses records") at Location("MTBFile",patId.value,"diagnoses")))
          andThen (_ validateEach),

        (previousGuidelineTherapies ifUndefined (Warning("Missing previous Guideline Therapies") at Location("MTBFile",patId.value,"previousGuidelineTherapies")))
          andThen (_ ifEmpty (Warning("Missing previous Guideline Therapies") at Location("MTBFile",patId.value,"previousGuidelineTherapies")))
          andThen (_ validateEach),

        (lastGuidelineTherapy ifUndefined (Error("Missing last Guideline Therapy") at Location("MTBFile",patId.value,"lastGuidelineTherapies")))
          andThen (_ validate),

        (ecogStatus ifUndefined (Warning("Missing ECOG Performance Status records") at Location("MTBFile",patId.value,"ecogStatus")))
          andThen (_ ifEmpty (Warning("Missing ECOG Performance Status records") at Location("MTBFile",patId.value,"ecogStatus")))
          andThen (_ validateEach),

        (specimens ifUndefined (Warning("Missing Specimen records") at Location("MTBFile",patId.value,"specimens")))
          andThen (_ ifEmpty (Warning("Missing Specimen records") at Location("MTBFile",patId.value,"specimens")))
          andThen (_ validateEach),

        (histologyResults ifUndefined (Warning("Missing HistologyResult records") at Location("MTBFile",patId.value,"histologyResults")))
          andThen (_ ifEmpty (Warning("Missing HistologyResult records") at Location("MTBFile",patId.value,"histologyResults")))
          andThen (_ validateEach),

        (carePlans ifUndefined (Warning("Missing CarePlan records") at Location("MTBFile",patId.value,"carePlans")))
          andThen (_ ifEmpty (Warning("Missing CarePlan records") at Location("MTBFile",patId.value,"carePlans")))
          andThen (_ validateEach),

        (recommendations ifUndefined (Warning("Missing TherapyRecommendation records") at Location("MTBFile",patId.value,"recommendations")))
          andThen (_ ifEmpty (Warning("Missing TherapyRecommendation records") at Location("MTBFile",patId.value,"recommendations")))
          andThen (_ validateEach),

        counsellingRequests.map(_ validateEach)
          .getOrElse(List.empty[GeneticCounsellingRequest].validNel[Issue]),

        rebiopsyRequests.map(_ validateEach)
          .getOrElse(List.empty[RebiopsyRequest].validNel[Issue]),

        (claims ifUndefined (Warning("Missing Insurance Claim records") at Location("MTBFile",patId.value,"claims")))
          andThen (_ ifEmpty (Warning("Missing Insurance Claim records") at Location("MTBFile",patId.value,"claims")))
          andThen (_ validateEach),

        (claimResponses ifUndefined (Warning("Missing ClaimResponse records") at Location("MTBFile",patId.value,"claimResponses")))
          andThen (_ ifEmpty (Warning("Missing ClaimResponse records") at Location("MTBFile",patId.value,"claimResponses")))
          andThen (_ validateEach),

        //TODO: validate Responses

      )
      .mapN { case _: Product => mtbfile}

    }


}
