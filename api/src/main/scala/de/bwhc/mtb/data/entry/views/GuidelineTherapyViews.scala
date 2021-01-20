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
  diagnosis: Diagnosis.Id,
  therapyLine: String Or TherapyLine,
  period: String Or PeriodDisplay[LocalDate],
  medication: NonEmptyList[MedicationDisplay],
  reasonStopped: String,
  response: String Or ResponseDisplay
)

object GuidelineTherapyView
{
  implicit val format = Json.writes[GuidelineTherapyView]
}


/*
case class PreviousGuidelineTherapyView
(
  id: TherapyId,
  diagnosis: Diagnosis.Id,
  therapyLine: String Or TherapyLine,
  medication: NonEmptyList[MedicationDisplay],
)


object PreviousGuidelineTherapyView
{
  implicit val format = Json.writes[PreviousGuidelineTherapyView]
}



case class LastGuidelineTherapyView
(
  id: TherapyId,
  diagnosis: Diagnosis.Id,
  therapyLine: String Or TherapyLine,
  period: String Or PeriodDisplay[LocalDate],
  medication: NonEmptyList[MedicationDisplay],
  reasonStopped: String
)

object LastGuidelineTherapyView
{
  implicit val format = Json.writes[LastGuidelineTherapyView]
}
*/
