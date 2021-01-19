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


case class PreviousGuidelineTherapyView
(
  id: TherapyId,
  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  therapyLine: String Or TherapyLine,
  medication: NonEmptyList[MedicationDisplay],
)
extends View[PreviousGuidelineTherapy]


object PreviousGuidelineTherapyView
{
  implicit val format = Json.writes[PreviousGuidelineTherapyView]
}



case class LastGuidelineTherapyView
(
  id: TherapyId,
  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  therapyLine: String Or TherapyLine,
//  period: PeriodDisplay[LocalDate],
  period: String Or PeriodDisplay[LocalDate],
  medication: NonEmptyList[MedicationDisplay],
  reasonStopped: String
)
extends View[LastGuidelineTherapy]

object LastGuidelineTherapyView
{
  implicit val format = Json.writes[LastGuidelineTherapyView]
}

