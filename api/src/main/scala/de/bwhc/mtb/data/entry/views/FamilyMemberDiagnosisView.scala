package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  FamilyMemberDiagnosis,
  Patient
}


final case class FamilyMemberDiagnosisView
(
  id: FamilyMemberDiagnosis.Id,
  relationship: String
)


object FamilyMemberDiagnosisView
{
  implicit val format = Json.writes[FamilyMemberDiagnosisView]
}
