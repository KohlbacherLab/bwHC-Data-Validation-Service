package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json



final case class MTBFile
(
  patient: Patient,
  consent: Consent,
  episode: MTBEpisode,
  diagnoses: Option[List[Diagnosis]],
  familyMemberDiagnoses: Option[List[FamilyMemberDiagnosis]],
  previousGuidelineTherapies: Option[List[PreviousGuidelineTherapy]],
  lastGuidelineTherapy: Option[LastGuidelineTherapy],
  ecogStatus: Option[List[ECOGStatus]],
  specimens: Option[List[Specimen]],
  molecularPathologyFindings: Option[List[MolecularPathologyFinding]],
  histologyReports: Option[List[HistologyReport]],
//  histologyResults: Option[List[HistologyResult]],
  ngsReports: Option[List[SomaticNGSReport]],
  carePlans: Option[List[CarePlan]],
  recommendations: Option[List[TherapyRecommendation]],
  geneticCounsellingRequests: Option[List[GeneticCounsellingRequest]],
  rebiopsyRequests: Option[List[RebiopsyRequest]],
  histologyReevaluationRequests: Option[List[HistologyReevaluationRequest]],
  studyInclusionRequests: Option[List[StudyInclusionRequest]],
  claims: Option[List[Claim]],
  claimResponses: Option[List[ClaimResponse]],
  molecularTherapies: Option[List[MolecularTherapyDocumentation]],
  responses: Option[List[Response]]
)

object MTBFile
{
  implicit val format = Json.format[MTBFile]
}
