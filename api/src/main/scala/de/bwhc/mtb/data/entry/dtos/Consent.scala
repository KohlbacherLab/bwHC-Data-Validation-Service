package de.bwhc.mtb.data.entry.dtos


import play.api.libs.json.Json



final case class Consent
(
  id: Consent.Id,
  patient: Patient.Id,
  status: Consent.Status.Value
)

object Consent
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]


  object Status extends Enumeration
  { 
    val Active   = Value("active")
    val Rejected = Value("rejected")

    implicit val format = Json.formatEnum(this)
  }

  implicit val format = Json.format[Consent]
}
