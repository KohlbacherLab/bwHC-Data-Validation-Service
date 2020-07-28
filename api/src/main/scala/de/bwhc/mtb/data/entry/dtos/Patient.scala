package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import play.api.libs.json.Json



object Patient
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[Patient]
}


final case class Patient
(
  id: Patient.Id,
  gender: Gender.Value,
  birthDate: Option[LocalDate],
  managingZPM: Option[ZPM],
  insurance: Option[HealthInsurance.Id],
  dateOfDeath: Option[LocalDate]
)
