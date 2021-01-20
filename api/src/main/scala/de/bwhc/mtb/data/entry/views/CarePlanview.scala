package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import cats.data.NonEmptyList

import play.api.libs.json.{Json,Writes, JsString}

import de.bwhc.mtb.data.entry.dtos._


sealed trait NoTarget
object NoTarget extends NoTarget
{
  implicit val format: Writes[NoTarget] = Writes(nt => JsString("Kein Target"))
}


case class CarePlanView
(
  id: CarePlan.Id,
//  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  icd10: ICD10Display,
  issuedOn: String Or LocalDate,
  protocol: String,
//  recommendations: NoTarget Or List[TherapyRecommendation.Id],
  geneticCounsellingRecommended: Boolean,
  rebiopsyRequests: String Or List[RebiopsyRequest.Id],
  inclusionInStudy: String Or List[NCTNumber]
)


object CarePlanView
{
  import de.bwhc.util.json._

  implicit val format = Json.writes[CarePlanView]
}
