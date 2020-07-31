package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate


import cats.data.NonEmptyList

import play.api.libs.json._
import de.bwhc.util.json._


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
  }


  implicit val formatNotDoneTherapy   = Json.format[NotDoneTherapy]
  implicit val formatOngoingTherapy   = Json.format[OngoingTherapy]
  implicit val formatStoppedTherapy   = Json.format[StoppedTherapy]
  implicit val formatCompletedTherapy = Json.format[CompletedTherapy]

  import MolecularTherapy.Status._

  implicit val format: Format[MolecularTherapy] =
    Format[MolecularTherapy](
      Reads(js =>
        (js \ "status")
          .validate[MolecularTherapy.Status.Value]
          .flatMap {
            case NotDone   => js.validate[NotDoneTherapy]
            case Stopped   => js.validate[StoppedTherapy]
            case Completed => js.validate[CompletedTherapy]
            case Ongoing   => js.validate[OngoingTherapy]
          }
      ),
      Writes {
        mth =>
          val js = mth match {
            case th: NotDoneTherapy   => Json.toJson(th)
            case th: StoppedTherapy   => Json.toJson(th)
            case th: CompletedTherapy => Json.toJson(th)
            case th: OngoingTherapy   => Json.toJson(th)
          }
          js.as[JsObject] + ("status" -> Json.toJson(mth.status))
      }
    )

}


sealed trait MolecularTherapy
{
  val id: TherapyId
  val patient: Patient.Id
  val status: MolecularTherapy.Status.Value
  val recordedOn: LocalDate
  val basedOn: TherapyRecommendation.Id
  val note: Option[String]
}

final case class NotDoneTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  basedOn: TherapyRecommendation.Id,
  notDoneReason: MolecularTherapy.NotDoneReason.Value,
  note: Option[String]
)
  extends MolecularTherapy
{
  val status = MolecularTherapy.Status.NotDone
}


final case class StoppedTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  basedOn: TherapyRecommendation.Id,
  period: ClosedPeriod[LocalDate],
  medication: NonEmptyList[Coding[Medication]],
  dosage: Option[Dosage.Value],
  reasonStopped: MolecularTherapy.StopReason.Value,
  note: Option[String]
)
  extends MolecularTherapy
{
  val status = MolecularTherapy.Status.Stopped
}


final case class CompletedTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  basedOn: TherapyRecommendation.Id,
  period: ClosedPeriod[LocalDate],
  medication: NonEmptyList[Coding[Medication]],
  dosage: Option[Dosage.Value],
  note: Option[String]
)
  extends MolecularTherapy
{
  val status = MolecularTherapy.Status.Completed
}


final case class OngoingTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  basedOn: TherapyRecommendation.Id,
  period: OpenEndPeriod[LocalDate],
  medication: NonEmptyList[Coding[Medication]],
  dosage: Option[Dosage.Value],
  note: Option[String]
)
  extends MolecularTherapy
{
  val status = MolecularTherapy.Status.Ongoing
}




case class MolecularTherapyDocumentation
(
  history: List[MolecularTherapy]
)

object MolecularTherapyDocumentation
{
  implicit val format = Json.format[MolecularTherapyDocumentation]
}
