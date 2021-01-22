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
  status: String,
  recordedOn: LocalDate,
  recommendation: TherapyRecommendation.Id,
  period: NotAvailable Or PeriodDisplay[LocalDate],
  notDoneReason: String,
  medication: List[MedicationDisplay],
  reasonStopped: String,
  dosage: NotAvailable Or Dosage.Value,
  note: String,
  response: NotAvailable Or ResponseDisplay
)


object MolecularTherapyView
{
  implicit val format = Json.writes[MolecularTherapyView]
}
