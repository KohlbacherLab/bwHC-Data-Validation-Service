package de.bwhc.mtb.data.entry.dtos



import java.time.{YearMonth,LocalDate}

import play.api.libs.json.{
  Json,
  JsString,
  Format,
  Reads,
  Writes
}


final case class Patient
(
  id: Patient.Id,
  gender: Gender.Value,
  birthDate: Option[YearMonth],
  managingZPM: Option[ZPM],
  insurance: Option[HealthInsurance.Id],
  dateOfDeath: Option[YearMonth]
)


object Patient
{

  case class Id(value: String) extends AnyVal

  import de.bwhc.util.json.time._


  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[Patient]
}
