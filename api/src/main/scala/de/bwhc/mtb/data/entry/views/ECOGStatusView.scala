package de.bwhc.mtb.data.entry.views


import java.time.LocalDate

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  ECOGStatus,
  ECOG
}

import play.api.libs.json.Json



final case class ECOGDisplay(value: String) extends AnyVal
object ECOGDisplay
{
  implicit val format = Json.valueFormat[ECOGDisplay]
}


final case class ECOGStatusView
(
  patient: Patient.Id,
  values: List[DatedValue[String,ECOGDisplay]]
)

object ECOGStatusView
{
  implicit val format = Json.format[ECOGStatusView]
}
