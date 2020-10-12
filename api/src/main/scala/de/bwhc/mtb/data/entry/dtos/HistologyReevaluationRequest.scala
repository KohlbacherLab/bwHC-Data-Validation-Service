package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

import play.api.libs.json.Json



final case class HistologyReevaluationRequest
(
  id: HistologyReevaluationRequest.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  issuedOn: Option[LocalDate]
)


object HistologyReevaluationRequest
{

  final case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[HistologyReevaluationRequest]

}
