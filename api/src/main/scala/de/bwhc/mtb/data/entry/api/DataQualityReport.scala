package de.bwhc.mtb.data.entry.api


import java.time.Instant

import cats.data.NonEmptyList

import play.api.libs.json._

import de.bwhc.util.json._

import de.bwhc.mtb.data.entry.dtos.Patient



case class DataQualityReport
(
  patient: Patient.Id,
  issues: NonEmptyList[DataQualityReport.Issue],
  createdAt: Instant = Instant.now
)


object DataQualityReport
{

  case class Issue
  (
    severity: Issue.Severity.Value,
    message: String,
    location: Issue.Location
  )

  object Issue
  {

    object Severity extends Enumeration
    {
      val Info    = Value("info")
      val Warning = Value("warning")
      val Error   = Value("error")
      val Fatal   = Value("fatal")
    
      implicit val format = Json.formatEnum(this)
    }
    
    case class Location
    (
      entryType: String,
      id: String,
      attribute: String
    )


    sealed trait Builder
    {
      def at(loc: Location): Issue
      def @@(loc: Location) = at(loc)
    }   

    private case class BuilderImpl
    (
      sev: Severity.Value,
      msg: String
    )
    extends Builder
    {
      def at(loc: Location): Issue = Issue(sev,msg,loc)
    }


    def Info(msg: String): Builder =
      BuilderImpl(Severity.Info,msg)

    def Warning(msg: String): Builder =
      BuilderImpl(Severity.Warning,msg)

    def Error(msg: String): Builder =
      BuilderImpl(Severity.Error,msg)

    def Fatal(msg: String): Builder =
      BuilderImpl(Severity.Fatal,msg)


    implicit val formatLocation = Json.format[Location]
    
    implicit val format = Json.format[Issue]

  }

  implicit val format = Json.format[DataQualityReport]

}
