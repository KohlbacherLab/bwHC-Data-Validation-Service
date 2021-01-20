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
  period: String Or PeriodDisplay[LocalDate],
  notDoneReason: String,
  medication: List[MedicationDisplay],
  reasonStopped: String,
  dosage: String Or Dosage.Value,
  note: String,
  response: String Or ResponseDisplay
)


object MolecularTherapyView
{
  implicit val format = Json.writes[MolecularTherapyView]
}

