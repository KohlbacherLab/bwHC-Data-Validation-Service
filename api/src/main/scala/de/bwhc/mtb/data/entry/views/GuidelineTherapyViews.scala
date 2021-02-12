package de.bwhc.mtb.data.entry.views


import java.time.LocalDate
import java.time.temporal.Temporal

import cats.data.NonEmptyList

import play.api.libs.json.Json

import de.bwhc.util.json._

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  PreviousGuidelineTherapy,
  LastGuidelineTherapy,
  TherapyId,
  TherapyLine,
  Diagnosis,
}



case class GuidelineTherapyView
(
  id: TherapyId,
  patient: Patient.Id,
  diagnosis: NotAvailable Or ICD10Display,
  therapyLine: NotAvailable Or TherapyLine,
  period: NotAvailable Or PeriodDisplay[LocalDate],
  medication: NotAvailable Or MedicationDisplay,
  reasonStopped: NotAvailable Or String,
  response: NotAvailable Or ResponseDisplay,
  progressionDate: NoValue Or LocalDate
)

object GuidelineTherapyView
{
  implicit val format = Json.writes[GuidelineTherapyView]
}

