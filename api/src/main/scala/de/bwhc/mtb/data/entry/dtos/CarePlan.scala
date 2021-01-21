package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

import play.api.libs.json.Json

import cats.data.NonEmptyList

import de.bwhc.util.json._



case class NoTargetFinding
(
  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  issuedOn: Option[LocalDate]
)

object NoTargetFinding
{
  implicit val format = Json.format[NoTargetFinding]
}


case class CarePlan
(
  id: CarePlan.Id,
  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  issuedOn: Option[LocalDate],
  description: Option[String],
//  result: Either[NoTargetFinding,NonEmptyList[TherapyRecommendation.Id]] //TODO: consider this for result modelling 
  noTargetFinding: Option[NoTargetFinding],
  recommendations: Option[List[TherapyRecommendation.Id]],
  geneticCounsellingRequest: Option[GeneticCounsellingRequest.Id],
  rebiopsyRequests: Option[List[RebiopsyRequest.Id]],
  studyInclusionRequest: Option[StudyInclusionRequest.Id]
)


object CarePlan
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[CarePlan]
}
