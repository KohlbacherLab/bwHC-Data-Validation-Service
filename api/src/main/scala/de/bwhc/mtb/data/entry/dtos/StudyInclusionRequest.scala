package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

import play.api.libs.json.Json



final case class NCTNumber(value: String) extends AnyVal
object NCTNumber
{
  implicit val format = Json.valueFormat[NCTNumber]
}


final case class StudyInclusionRequest
(
  id: StudyInclusionRequest.Id,
  patient: Patient.Id,
  reason: Diagnosis.Id,
  nctNumber: NCTNumber,
  issuedOn: Option[LocalDate]
)


object StudyInclusionRequest
{

  final case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[StudyInclusionRequest]

}
