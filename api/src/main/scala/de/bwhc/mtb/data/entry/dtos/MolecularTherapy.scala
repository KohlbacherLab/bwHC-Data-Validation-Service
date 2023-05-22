package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate
import cats.data.NonEmptyList
import play.api.libs.json._


object Dosage extends Enumeration
{
  val Under50Percent = Value("<50%")
  val Over50Percent  = Value(">=50%")

  implicit val format = Json.formatEnum(this)
}


object MolecularTherapy
{

  object Status extends Enumeration
  {
    val NotDone   = Value("not-done")
    val Ongoing   = Value("on-going")
    val Stopped   = Value("stopped")
    val Completed = Value("completed")
  
    implicit val format = Json.formatEnum(this)
    implicit val system = Coding.System[Value]("Molekular-Therapie-Status")
  }


  object NotDoneReason extends Enumeration
  {
    type NotDoneReason = Value

    val PaymentRefused      = Value("payment-refused")
    val PaymentPending      = Value("payment-pending")
    val NoIndication        = Value("no-indication")
    val MedicalReason       = Value("medical-reason")
    val PatientRefusal      = Value("patient-refusal")
    val PatientDeath        = Value("patient-death")
    val OtherTherapyChosen  = Value("other-therapy-chosen")
    val ContinuedExternally = Value("continued-externally")
    val LostToFU            = Value("lost-to-fu")
    val Other               = Value("other")
    val Unknown             = Value("unknown")
  
    implicit val format = Json.formatEnum(this)

    implicit val system = Coding.System[NotDoneReason.Value]("MTB-CDS:MolecularTherapy:NotDoneReason")
  }


  object StopReason extends Enumeration
  {
    type StopReason = Value

    val Remission           = Value("remission")
    val PatientWish         = Value("patient-wish")
    val PaymentEnded        = Value("payment-ended")
    val MedicalReason       = Value("medical-reason")
    val Progression         = Value("progression")
    val PatientDeath        = Value("patient-death")
    val Toxicity            = Value("toxicity")
    val OtherTherapyChosen  = Value("other-therapy-chosen")
    val ContinuedExternally = Value("continued-externally")
    val StateDeterioration  = Value("deterioration")
    val Other               = Value("other")
    val Unknown             = Value("unknown")
  
    implicit val format = Json.formatEnum(this)

    implicit val system = Coding.System[StopReason.Value]("MTB-CDS:MolecularTherapy:StopReason")
  }

  implicit val format = Json.format[MolecularTherapy]
}


final case class MolecularTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  status: MolecularTherapy.Status.Value,
  basedOn: TherapyRecommendation.Id,
  reason: Option[Coding[ICD10GM]],
  period: Option[Period[LocalDate]],
  medication: Option[List[Medication.Coding]],
  dosage: Option[Dosage.Value],
  //TODO: combine notDoneReason and reasonStopped to 'statusReason'
  notDoneReason: Option[Coding[MolecularTherapy.NotDoneReason.Value]],
  reasonStopped: Option[Coding[MolecularTherapy.StopReason.Value]],
  note: Option[String]
)


case class MolecularTherapyDocumentation
(
  history: List[MolecularTherapy]
)

object MolecularTherapyDocumentation
{
  implicit val format = Json.format[MolecularTherapyDocumentation]
}
