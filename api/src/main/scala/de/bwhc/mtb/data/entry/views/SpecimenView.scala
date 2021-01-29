package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  Specimen,
  ICD10GM
}



final case class SpecimenView
(
  id: Specimen.Id,
  patient: Patient.Id,
  icd10: ICD10Display,
  `type`:  NotAvailable Or String,
  collectionDate: NotAvailable Or LocalDate,
  localization:  NotAvailable Or String,
  collectionMethod:  NotAvailable Or String
)


object SpecimenView
{

  import de.bwhc.util.json._

  implicit val format = Json.writes[SpecimenView]

}
