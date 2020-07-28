package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

import play.api.libs.json.Json

import cats.data.NonEmptyList

import de.bwhc.util.json._



case class CarePlan
(
  id: CarePlan.Id,
  patient: Patient.Id,
  issuedOn: LocalDate,
  description: Option[String],
//  recommendations: NonEmptyList[TherapyRecommendation],
  recommendations: NonEmptyList[TherapyRecommendation.Id],
  geneticCounselling: Option[Boolean],
  rebiopsyRequests: List[RebiopsyRequest.Id]
)


object CarePlan
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[CarePlan]
}
