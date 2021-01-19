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
//  patient: Patient.Id,
  icd10: ICD10Display,
  `type`: String,
  collectionDate: String Or LocalDate,
  localization: String,
  collectionMethod: String
)


object SpecimenView
{

  import de.bwhc.util.json._

  implicit val format = Json.writes[SpecimenView]

}
