package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import play.api.libs.json.Json



final case class MTBEpisode
(
  id: MTBEpisode.Id,
  patient: Patient.Id,
  period: OpenEndPeriod[LocalDate]
)


object MTBEpisode
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]
  implicit val format   = Json.format[MTBEpisode]
}
