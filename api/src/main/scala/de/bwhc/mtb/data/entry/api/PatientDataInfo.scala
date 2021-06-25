package de.bwhc.mtb.data.entry.api


import java.time.YearMonth


import play.api.libs.json.Json

import de.bwhc.util.json._
import de.bwhc.util.json.time._

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  Gender,
}

import de.bwhc.mtb.data.entry.views.{
  Or,
  NotAvailable
}


final case class PatientDataInfo
(
  id: Patient.Id,
  gender: String,
  birthDate: NotAvailable Or YearMonth,
  numberOfIssues: Int,
//  issueDistribution: List[SeverityCount]
/*  
  errors: Int,
  warnings: Int,
  infos: Int
*/
)


object PatientDataInfo
{
/*
  final case class SeverityCount
  (
    severity: DataQualityReport.Issue.Severity.Value,
    count: Int
  ) 

  implicit val formatSeverityCount = Json.writes[SeverityCount]
*/
  implicit val format = Json.writes[PatientDataInfo]

}


