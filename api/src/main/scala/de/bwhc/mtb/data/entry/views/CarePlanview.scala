package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import cats.data.NonEmptyList

import play.api.libs.json.{Json,Writes, JsString}

import de.bwhc.util.json._

import de.bwhc.mtb.data.entry.dtos._


sealed trait NoTarget
object NoTarget extends NoTarget
{
  implicit val format: Writes[NoTarget] =
    Writes(nt => JsString("Kein Target"))
}


final case class LevelOfEvidenceDisplay(value: String) extends AnyVal
object LevelOfEvidenceDisplay
{
  implicit val format = Json.valueWrites[LevelOfEvidenceDisplay]
}


case class SupportingVariantDisplay(value: String) extends AnyVal
object SupportingVariantDisplay
{
  implicit val format = Json.valueWrites[SupportingVariantDisplay]
}


final case class NCTNumbersDisplay(value: String) extends AnyVal
object NCTNumbersDisplay
{
  implicit val format = Json.valueWrites[NCTNumbersDisplay]
}


final case class TherapyRecommendationView
(
  id: TherapyRecommendation.Id,
  patient: Patient.Id,
  icd10: NotAvailable Or ICD10Display,
  medication: NotAvailable Or MedicationDisplay,
  priority: NotAvailable Or TherapyRecommendation.Priority.Value,
  levelOfEvidence: NotAvailable Or LevelOfEvidenceDisplay,
  supportingVariants: List[SupportingVariantDisplay]
)

object TherapyRecommendationView
{
  implicit val format = Json.writes[TherapyRecommendationView]
}



final case class CarePlanView
(
  id: CarePlan.Id,
  patient: Patient.Id,
  icd10: NotAvailable Or ICD10Display,
  issuedOn: NotAvailable Or LocalDate,
  protocol: NotAvailable Or String,
  geneticCounsellingRecommendation: No Or String,
  inclusionInStudyRecommendation: NotAvailable Or NCTNumbersDisplay,
//  inclusionInStudyRecommendation: NotAvailable Or NCTNumber,
  targetAvailable: Yes Or No,
  therapyRecommendations: List[TherapyRecommendationView],
  rebiopsyRequests: NotAvailable Or List[RebiopsyRequest.Id],
)

object CarePlanView
{
  implicit val format = Json.writes[CarePlanView]
}

