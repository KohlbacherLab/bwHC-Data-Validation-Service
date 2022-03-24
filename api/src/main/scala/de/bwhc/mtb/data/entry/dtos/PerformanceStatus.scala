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


/*
object ECOG extends Enumeration
{
  val Zero  = Value("LA9622-7")
  val One   = Value("LA9623-5")
  val Two   = Value("LA9624-3")
  val Three = Value("LA9625-0")
  val Four  = Value("LA9626-8")
  val Five  = Value "LA9627-6")

  implicit val format = Json.formatEnum(this)

  implicit val system = Coding.System[ECOG.Value]("ECOG-Performance-Status")
}
*/


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
