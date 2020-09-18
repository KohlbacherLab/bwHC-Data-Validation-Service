package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import play.api.libs.json.Json



object ECOG extends Enumeration
{
  val Zero  = Value("0")
  val One   = Value("1")
  val Two   = Value("2")
  val Three = Value("3")
  val Four  = Value("4")

  implicit val format = Json.formatEnum(this)

  implicit val system = Coding.System[ECOG.Value]("ECOG-Performance-Status")
}


final case class ECOGStatus
(
  id: ECOGStatus.Id,
  patient: Patient.Id,
  effectiveDate: Option[LocalDate],
  value: Coding[ECOG.Value]
) 


object ECOGStatus
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format   = Json.format[ECOGStatus]
}
