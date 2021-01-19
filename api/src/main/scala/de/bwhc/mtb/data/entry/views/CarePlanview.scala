package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import cats.data.NonEmptyList

import play.api.libs.json.{Json,Writes, JsString}

import de.bwhc.mtb.data.entry.dtos._


sealed trait NoTarget
{
  override def toString = "Kein Target"
}
object NoTarget extends NoTarget
{
  implicit val format: Writes[NoTarget] = Writes(nt => JsString(nt.toString))
}


case class CarePlanView
(
  id: CarePlan.Id,
//  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  issuedOn: String Or LocalDate,
  protocol: String,
//  recommendations: NoTarget Or NonEmptyList[TherapyRecommendation.Id],
  recommendations: NoTarget Or List[TherapyRecommendation.Id],
  geneticCounsellingRequest: String Or GeneticCounsellingRequest.Id,
  rebiopsyRequests: String Or List[RebiopsyRequest.Id]
)


object CarePlanView
{
  import de.bwhc.util.json._

  implicit val format = Json.writes[CarePlanView]
}
