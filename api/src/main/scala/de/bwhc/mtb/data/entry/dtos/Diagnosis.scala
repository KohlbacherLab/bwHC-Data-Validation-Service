package de.bwhc.mtb.data.entry.dtos



import java.time.{
  LocalDate,LocalDateTime
}

import cats.data.NonEmptyList

import play.api.libs.json.Json



case class ICD10GM(value: String) extends AnyVal
object ICD10GM
{
  implicit val format = Json.valueFormat[ICD10GM]
  implicit val system = Coding.System[ICD10GM]("ICD-10-GM")
}

case class ICDO3T(value: String) extends AnyVal
object ICDO3T
{
  implicit val format = Json.valueFormat[ICDO3T]
  implicit val system = Coding.System[ICDO3T]("ICD-O-3-T")
}


final case class Diagnosis
(
  id: Diagnosis.Id,
  patient: Patient.Id,
  recordedOn: Option[LocalDate],
  icd10: Option[Coding[ICD10GM]],
  icdO3T: Option[Coding[ICDO3T]],
  whoGrade: Option[Coding[WHOGrade.Value]],
  histologyResults: Option[List[HistologyResult.Id]],
  statusHistory: Option[List[Diagnosis.StatusOnDate]]
)

object Diagnosis
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]


  object Status extends Enumeration
  {
    type Status = Value
  
    val TumorFree    = Value("tumor-free")
    val Local        = Value("local")
    val Metastasized = Value("metastasized")
    val Unknown      = Value("unknown")

    implicit val format = Json.formatEnum(this)
  }


  case class StatusOnDate
  (
    status: Status.Value,
    date: LocalDate
  )

  implicit val formatStatusOnDate = Json.format[StatusOnDate]

  implicit val format = Json.format[Diagnosis]

}
