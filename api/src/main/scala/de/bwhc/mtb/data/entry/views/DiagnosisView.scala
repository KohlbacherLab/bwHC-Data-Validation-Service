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
}


case class ICDO3TDisplay(value: String) extends AnyVal
object ICDO3TDisplay
{
  implicit val format = Json.valueFormat[ICDO3TDisplay]
}


case class WHOGradeDisplay(value: String) extends AnyVal
object WHOGradeDisplay
{
  implicit val format = Json.valueFormat[WHOGradeDisplay]
}



final case class DiagnosisView
(
  id: Diagnosis.Id,
  patient: Patient.Id,
  recordedOn: NotAvailable Or LocalDate,
  icd10: NotAvailable Or ICD10Display,
  icdO3T: NotAvailable Or ICDO3TDisplay,
  whoGrade: NotAvailable Or WHOGradeDisplay,
  //TODO: histologyReports
  //TODO: statusHistory,
  statusHistory: NotAvailable Or String,
  guidelineTreatmentStatus: NotAvailable Or String 
)


object DiagnosisView
{
  import de.bwhc.util.json._

  implicit val format = Json.writes[DiagnosisView]
}
