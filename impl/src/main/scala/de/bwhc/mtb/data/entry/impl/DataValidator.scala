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

  import cats.implicits._

  import cats.data.Validated
  import Validated._

  import DataQualityReport._
  import DataQualityReport.Issue._


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



  implicit val icd10gm = ICD10GMCatalogs.getInstance.get

  implicit val icdO3   = ICDO3Catalogs.getInstance.get


  implicit def icd10Validator(
    implicit
//    diagId: Diagnosis.Id,
    catalog: ICD10GMCatalogs
  ): DataQualityValidator[Coding[ICD10GM]] = {

      case icd10 @ Coding(code,display,version) =>

//        (version ifUndefined (Error("Missing ICD-10-GM Version") at Location("Diagnosis",diagId.value,"ICD-10 Coding")))
        (version ifUndefined (Error("Missing ICD-10-GM Version") at Location("ICD-10-GM Coding","","version")))
          .andThen (
            v =>
              ifThrows(
                icd.ICD10GM.Version(v)
              )(
                Error(s"Invalid ICD-10-GM Version $version") at Location("ICD-10-GM Coding","","version")
//                Error(s"Invalid ICD-10-GM Version $version") at Location("Diagnosis",diagId.value,"ICD-10 Coding")
              )
          )
          .andThen (
            v =>
              code.value mustBeIn catalog.codings(v).map(_.code.value)
                otherwise (Error(s"Invalid ICD-10-GM code $code") at Location("ICD-10-GM Coding","","code"))
//                otherwise (Error(s"Invalid ICD-10-GM code $code") at Location("Diagnosis",diagId.value,"ICD-10 Coding"))
          )
          .map(c => icd10)

    }

  implicit def icdO3TValidator(
    implicit
    diagId: Diagnosis.Id,
    catalog: ICDO3Catalogs
  ): DataQualityValidator[Coding[ICDO3T]] = {

      case icdo3t @ Coding(code,display,version) =>

        (version ifUndefined (Error("Missing ICD-O-3 Version") at Location("Diagnosis",diagId.value,"ICD-O-3-T Coding")))
          .andThen(
            v =>
              ifThrows(icd.ICDO3.Version(v))(Error(s"Invalid ICD-O-3 Version $version") at Location("Diagnosis",diagId.value,"ICD-O-3-T Coding"))
          )
          .andThen(
            v =>
              code.value mustBeIn catalog.topographyCodings(v).map(_.code.value)
                otherwise (Error(s"Invalid ICD-O-3-T code $code") at Location("Diagnosis",diagId.value,"ICD-O-3-T Coding"))
          )
          .map(c => icdo3t)

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
    histologyRefs: List[HistologyResult.Id]
  ): DataQualityValidator[Diagnosis] = {

    case diag @ Diagnosis(Diagnosis.Id(id),patient,date,icd10,icdO3T,_,optHistology,_) =>

      implicit val diagId = diag.id

      (
        (patient mustBe patId otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("Diagnosis",id,"patient"))),

        (date ifUndefined (Warning("Missing Recording Date") at Location("Diagnosis",id,"recordedOn"))),

        (icd10 ifUndefined (Error("Missing ICD-10-GM Coding") at Location("Diagnosis",id,"ICD-10 Coding")))
          andThen (_ validate),

        (icdO3T ifUndefined (Warning("Missing ICD-O-3-T Coding") at Location("Diagnosis",id,"ICD-O-3-T Coding")))
          andThen (_ validate),

        (optHistology ifUndefined (Warning("Missing Histology results") at Location("Diagnosis",id,"ICD-O-3-T Coding")))
          andThen (
            _.map( ref =>
              ref mustBeIn histologyRefs
                otherwise (Error(s"Invalid Reference HistologyResult/${ref.value}") at Location("Diagnosis",id,"histologyResults"))
            ).sequence
          ) 

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
        //TODO: validate medication against catalog
        
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
        
        (medication ifUndefined (Warning("Missing Medication") at Location("LastGuidelineTherapy",id,"medication"))),
        //TODO: validate medication against catalog

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
      _,
      histologyResults,
      _,
      _,
      _,
      _,
      _,
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
        histologyResults.getOrElse(List.empty[HistologyResult])
          .map(_.id)


      implicit val therapyRefs =
        responses.getOrElse(List.empty[Response])
          .map(_.therapy)


      (
        patient.validate,

        consent.validate,

        episode.validate,

        (diagnoses ifUndefined (Error("Missing diagnosis records") at Location("MTBFile",patId.value,"diagnoses")))
          andThen (_ ifEmpty (Error("Missing diagnoses records") at Location("MTBFile",patId.value,"diagnoses")))
          andThen (_ validate),

        (previousGuidelineTherapies ifUndefined (Warning("Missing previous Guideline Therapies") at Location("MTBFile",patId.value,"previousGuidelineTherapies")))
          andThen (_ ifEmpty (Warning("Missing previous Guideline Therapies") at Location("MTBFile",patId.value,"previousGuidelineTherapies")))
          andThen (_ validate),

        (lastGuidelineTherapy ifUndefined (Error("Missing last Guideline Therapy") at Location("MTBFile",patId.value,"lastGuidelineTherapies")))
          andThen (_ validate),

        (ecogStatus ifUndefined (Warning("Missing ECOG Performance Status records") at Location("MTBFile",patId.value,"ecogStatus")))
          andThen (_ ifEmpty (Warning("Missing ECOG Performance Status records") at Location("MTBFile",patId.value,"ecogStatus")))
          andThen (_ validate),

      )
      .mapN { case _: Product => mtbfile}

    }


}
