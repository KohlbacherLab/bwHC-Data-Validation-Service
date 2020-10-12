package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import play.api.libs.json.Json


final case class PathologyDept(value: String) extends AnyVal
object PathologyDept
{
  implicit val format = Json.valueFormat[PathologyDept]
}


final case class MolecularPathologyFinding
(
  id: MolecularPathologyFinding.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  performingInstitute: Option[PathologyDept],
  issuedOn: Option[LocalDate],
  note: String
)


object MolecularPathologyFinding
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]
  implicit val format   = Json.format[MolecularPathologyFinding]
}
