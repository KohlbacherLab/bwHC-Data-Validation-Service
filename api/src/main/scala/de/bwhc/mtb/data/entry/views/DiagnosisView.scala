package de.bwhc.mtb.data.entry.views



import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.{
  Diagnosis,
  Patient,
  Coding,
  ICD10GM,
  ICDO3T,
  GuidelineTreatmentStatus
}


case class ICD10Display(value: String) extends AnyVal

object ICD10Display
{
  implicit val format = Json.valueFormat[ICD10Display]

  implicit val dflt: Default[ICD10Display] = Default(ICD10Display("N/A"))
}


case class ICDO3TDisplay(value: String) extends AnyVal

object ICDO3TDisplay
{
  implicit val format = Json.valueFormat[ICDO3TDisplay]

  implicit val dflt: Default[ICDO3TDisplay] = Default(ICDO3TDisplay("-"))
}



final case class DiagnosisView
(
  id: Diagnosis.Id,
  patient: Patient.Id,
  recordedOn: String Or LocalDate,
  icd10: ICD10Display,
  icdO3T: ICDO3TDisplay,
//  icd10: String,
//  icdO3T: String,
  whoGrade: String,
  //TODO: statusHistory, histologyReports
  guidelineTreatmentStatus: String 
)


object DiagnosisView
{
  import de.bwhc.util.json._

  implicit val format = Json.writes[DiagnosisView]
}
