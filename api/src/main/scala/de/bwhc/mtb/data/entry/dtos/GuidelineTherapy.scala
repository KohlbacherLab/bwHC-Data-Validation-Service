package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate
import play.api.libs.json.Json



final case class TherapyLine(value: Int) extends AnyVal
object TherapyLine
{

  val values = (1 to 9).map(TherapyLine(_)).toList

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

    implicit val system = Coding.System[StopReason.Value]("MTB-CDS:GuidelineTherapy-StopReason")
  }

}


sealed trait GuidelineTherapy
{
  val id: TherapyId
  val patient: Patient.Id
  val diagnosis: Diagnosis.Id
  val therapyLine: Option[TherapyLine]
  val medication: Option[List[Medication.Coding]]
}


case class PreviousGuidelineTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  therapyLine: Option[TherapyLine],
  medication: Option[List[Medication.Coding]]
)
extends GuidelineTherapy

object PreviousGuidelineTherapy
{
  implicit val format = Json.format[PreviousGuidelineTherapy]
}


case class LastGuidelineTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  therapyLine: Option[TherapyLine],
  period: Option[OpenEndPeriod[LocalDate]],
  medication: Option[List[Medication.Coding]],
  reasonStopped: Option[Coding[GuidelineTherapy.StopReason.Value]]
)
extends GuidelineTherapy
  
object LastGuidelineTherapy
{
  implicit val format = Json.format[LastGuidelineTherapy]
}
