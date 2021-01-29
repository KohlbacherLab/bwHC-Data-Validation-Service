package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  Gender,
  HealthInsurance,
  Consent,
  ZPM
}


final case class PatientView
(
  id: Patient.Id,
  gender: String,
  birthDate: NotAvailable Or LocalDate,
  managingZPM: NotAvailable Or ZPM,
  insurance: NotAvailable Or HealthInsurance.Id,
  dateOfDeath: NotAvailable Or LocalDate,
  constentStatus: Consent.Status.Value,
  firstReferralDate: LocalDate
)


object PatientView
{

  import de.bwhc.util.json._

  implicit val format = Json.writes[PatientView]
}


