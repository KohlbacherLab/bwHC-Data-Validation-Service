package de.bwhc.mtb.data.entry.views



import java.time.LocalDate


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

/*
final case class LevelOfEvidenceAddendums(value: String) extends AnyVal
object LevelOfEvidenceAddendums
{
  implicit val format = Json.valueWrites[LevelOfEvidenceAddendums]
}
*/

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
  ecogStatus: NotAvailable Or ECOGDisplay,
  medication: NotAvailable Or MedicationDisplay,
  medicationClasses: NotAvailable Or MedicationDisplay,
  priority: NotAvailable Or TherapyRecommendation.Priority.Value,
  levelOfEvidence: NotAvailable Or LevelOfEvidenceDisplay,
//  levelOfEvidenceGrading: NotAvailable Or LevelOfEvidenceDisplay,
//  levelOfEvidenceAddendums: NotAvailable Or LevelOfEvidenceDisplay,
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
  targetAvailable: Yes Or No,
  therapyRecommendations: List[TherapyRecommendationView],
  rebiopsyRequests: NotAvailable Or List[RebiopsyRequest.Id],
)

object CarePlanView
{
  implicit val format = Json.writes[CarePlanView]
}

