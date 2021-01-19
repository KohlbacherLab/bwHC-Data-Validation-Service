package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  Gender,
  HealthInsurance,
  ZPM
}


final case class PatientView
(
  id: Patient.Id,
  gender: String,
  birthDate: String Or LocalDate,
  managingZPM: String Or ZPM,
  insurance: String Or HealthInsurance.Id,
  dateOfDeath: String Or LocalDate,
)
extends View[Patient]


object PatientView
{

  import de.bwhc.util.json._

  implicit val format = Json.writes[PatientView]
}


