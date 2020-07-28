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

/*
  implicit def periodValidator[T <: Temporal]: DataQualityValidator[Period[T]] = {
    case open @ OpenEndPeriod(start,end) =>

    case closed @ ClosedPeriod(start,end) =>
  }
*/

  implicit val patientQC: DataQualityValidator[Patient] = {

    case pat @ Patient(Patient.Id(id),_,birthDate,_,insurance,dod) =>

      (
        birthDate ifUndefined (Error("Missing BirthDate") at Location("Patient",id,"birthdate")),

        insurance ifUndefined (Warning("Missing Health Insurance") at Location("Patient",id,"insurance")),

        dod ifUndefined (Info("Undefined date of death. Ensure if up to date") at Location("Patient",id,"dateOfDeath"))
      )
      .mapN { case _: Product => pat}
  }


  implicit def consentQC(
    implicit patId: Patient.Id
  ): DataQualityValidator[Consent] = {
    case consent @ Consent(id,patient,_) =>

      ((patient mustBe patId)
        otherwise (Error(s"Invalid Reference to Patient/${patId.value}") at Location("Consent",id.value,"patient")))
        .map(_ => consent)

  }


  implicit def episodeQC(
    implicit patId: Patient.Id
  ): DataQualityValidator[MTBEpisode] = {
    case episode @ MTBEpisode(id,patient,period) =>
      condNel(
        patient == patId,
        episode,
        Error(s"Invalid Reference to Patient/${patId.value}") at Location("MTBEpisode",id.value,"patient")
      )
      
  }



  implicit val icd10gm = ICD10GMCatalogs.getInstance.get

  implicit val icdO3   = ICDO3Catalogs.getInstance.get


  implicit def icd10QC(
    implicit
    diagId: Diagnosis.Id,
    catalog: ICD10GMCatalogs
  ): DataQualityValidator[Coding[ICD10GM]] = {

      case icd10 @ Coding(code,display,version) =>

        (version ifUndefined (Error("Missing ICD-10-GM Version") at Location("Diagnosis",diagId.value,"ICD-10 Coding")))
          .andThen(
            v =>
              ifThrows(
                icd.ICD10GM.Version(v)
              )(
                Error(s"Invalid ICD-10-GM Version $version") at Location("Diagnosis",diagId.value,"ICD-10 Coding")
              )
          )
          .andThen(
            v =>
              (code.value mustBeIn catalog.codings(v).map(_.code.value))
                .otherwise(Error(s"Invalid ICD-10-GM code $code") at Location("Diagnosis",diagId.value,"ICD-10 Coding"))
          )
          .map(c => icd10)

    }

  implicit def icdO3TQC(
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
              (code.value mustBeIn catalog.topographyCodings(v).map(_.code.value))
                .otherwise(Error(s"Invalid ICD-O-3-T code $code") at Location("Diagnosis",diagId.value,"ICD-O-3-T Coding"))
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

  implicit def diagnosisQC(
    implicit
    histoRefs: Option[List[HistologyResult.Id]]
  ): DataQualityValidator[Diagnosis] = {

    case diag @ Diagnosis(Diagnosis.Id(id),pat,date,icd10,icdO3T,_,optHistology,_) =>

      implicit val diagId = diag.id

      (
        (date ifUndefined (Warning("Missing Recording Date") at Location("Diagnosis",id,"recordedOn"))),

        (icd10 ifUndefined (Error("Missing ICD-10-GM Coding") at Location("Diagnosis",id,"ICD-10 Coding")))
          andThen (_ validate),

        (icdO3T ifUndefined (Warning("Missing ICD-O-3-T Coding") at Location("Diagnosis",id,"ICD-O-3-T Coding")))
          andThen (_ validate),

        (optHistology ifUndefined (Warning("Missing Histology results") at Location("Diagnosis",id,"ICD-O-3-T Coding")))
          andThen (
            _.map( ref =>
              ref mustBeIn histoRefs.getOrElse(List.empty[HistologyResult.Id])
                otherwise (Error(s"Invalid Reference HistologyResult/${ref.value}") at Location("Diagnosis",id,"histologyResults"))
            ).sequence
          ) 

      )
      .mapN { case _: Product => diag}

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

  implicit val mtbFileQC: DataQualityValidator[MTBFile] = {

    case mtbfile @ MTBFile(
      patient,
      consent,
      episode,
      diags,
      _,
      _,
      _,
      _,
      _,
      histologyResults,
      _,
      _,
      _,
      _,
      _,
      _,
      _
    ) =>
    
      implicit val patId = patient.id  

      implicit val histoRefs = histologyResults.map(_.map(_.id))

      (
        patient.validate,
        consent.validate,
        episode.validate,
        (diags ifUndefined (Error("Missing diagnosis records") at Location("MTBFile",patId.value,"Diagnoses")))
          andThen (_ ifEmpty (Error("Missing diagnoses records") at Location("MTBFile",patId.value,"Diagnoses")))
          andThen (_ validate)

      )
      .mapN { case _: Product => mtbfile}

    }


}
