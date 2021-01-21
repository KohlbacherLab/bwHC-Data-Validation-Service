package de.bwhc.mtb.data.entry.views


import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  Specimen,
  MolecularPathologyFinding,
  PathologyDept
}



final case class MolecularPathologyFindingView
(
  id: MolecularPathologyFinding.Id,
//  patient: Patient.Id,
  specimen: Specimen.Id,
  performingInstitute: NotAvailable Or PathologyDept,
  issuedOn: NotAvailable Or LocalDate,
  note: String
)

object MolecularPathologyFindingView
{
  import de.bwhc.util.json._

  implicit val format = Json.writes[MolecularPathologyFindingView]
}
