package de.bwhc.mtb.data.entry.views


import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos._


final case class MTBFileView
(
  patient: PatientView,
  diagnoses: List[DiagnosisView],
  familyMemberDiagnoses: List[FamilyMemberDiagnosisView],
  previousGuidelineTherapies: List[PreviousGuidelineTherapyView],
  lastGuidelineTherapy: Option[LastGuidelineTherapyView],
  //TODO: ecogStatus
  specimens: List[SpecimenView],
  molecularPathologyFindings: List[MolecularPathologyFindingView],
  histologyReports: List[HistologyReportView],
  //TODO: NGSReports
  

)
extends View[MTBFile]


object MTBFileView
{
  implicit val format = Json.writes[MTBFileView]
}
