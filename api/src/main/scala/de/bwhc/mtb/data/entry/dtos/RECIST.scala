package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import play.api.libs.json.Json


object RECIST extends Enumeration
{
  val CR  = Value("CR")
  val PR  = Value("PR")
  val MR  = Value("MR")
  val SD  = Value("SD")
  val PD  = Value("PD")
  val NA  = Value("NA")
  val NYA = Value("NYA")

  implicit val format = Json.formatEnum(this)

  implicit val system = Coding.System[RECIST.Value]("RECIST")
}



final case class Response
(
  id: Response.Id,
  therapy: TherapyId,
  effectiveDate: LocalDate,
  value: Coding[RECIST.Value]
)


object Response
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[Response]
}
