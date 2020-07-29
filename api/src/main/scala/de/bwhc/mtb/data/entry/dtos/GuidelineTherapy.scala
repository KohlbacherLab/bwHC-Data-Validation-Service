package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

import play.api.libs.json.Json

import cats.data.NonEmptyList



final case class TherapyLine(value: Int) extends AnyVal
object TherapyLine
{
  implicit val format = Json.valueFormat[TherapyLine]
}


object GuidelineTherapy
{

  object StopReason extends Enumeration
  {
    type StopReason = Value

    val PatientWish        = Value("patient-wish")
    val Progression        = Value("progression")
    val Toxicity           = Value("toxicity")
    val StateDeterioration = Value("deterioration")
    val Remission          = Value("chronic-remission")
    val Other              = Value("other")
    val Unknown            = Value("unknown")

    implicit val format = Json.formatEnum(this)
  }

}


sealed trait GuidelineTherapy
{
  val id: TherapyId
  val patient: Patient.Id
  val therapyLine: Option[TherapyLine]
  val medication: Option[List[Coding[Medication]]]
}


case class PreviousGuidelineTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  therapyLine: Option[TherapyLine],
  medication: Option[List[Coding[Medication]]],
) extends GuidelineTherapy

object PreviousGuidelineTherapy
{
  implicit val format = Json.format[PreviousGuidelineTherapy]
}


case class LastGuidelineTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  therapyLine: Option[TherapyLine],
  period: Option[OpenEndPeriod[LocalDate]],
  medication: Option[List[Coding[Medication]]],
  reasonStopped: Option[GuidelineTherapy.StopReason.Value]
) extends GuidelineTherapy
  
object LastGuidelineTherapy
{
  implicit val format = Json.format[LastGuidelineTherapy]
}
