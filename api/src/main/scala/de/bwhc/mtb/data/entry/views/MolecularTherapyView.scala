package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.util.json._

import de.bwhc.mtb.data.entry.dtos.{
  TherapyId,
  TherapyRecommendation,
  Patient,
  MolecularTherapy,
  Dosage
}


final case class MolecularTherapyView
(
  id: TherapyId,
  patient: Patient.Id,
  diagnosis: NotAvailable Or ICD10Display,
  status: String,
  recordedOn: LocalDate,
  recommendation: TherapyRecommendation.Id,
  recommendationPriority: NotAvailable Or TherapyRecommendation.Priority.Value,
  period: NotAvailable Or PeriodDisplay[LocalDate],
  notDoneReason: NoValue Or String,
  medication: NoValue Or MedicationDisplay,
  supportingVariants: List[SupportingVariantDisplay],
  reasonStopped: NoValue Or String,
  dosage: NotAvailable Or Dosage.Value,
  note: String,
  response: NotAvailable Or ResponseDisplay,
  progressionDate: NoValue Or LocalDate
)


object MolecularTherapyView
{
  implicit val format = Json.writes[MolecularTherapyView]
}

