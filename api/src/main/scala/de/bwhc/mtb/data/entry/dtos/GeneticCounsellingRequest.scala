package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

import play.api.libs.json.Json



final case class GeneticCounsellingRequest
(
  id: GeneticCounsellingRequest.Id,
  patient: Patient.Id,
  issuedOn: Option[LocalDate],
  reason: String
)


object GeneticCounsellingRequest
{
  final case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[GeneticCounsellingRequest]

}
