package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import play.api.libs.json.Json


case class ICDO3M(value: String) extends AnyVal
object ICDO3M
{
  implicit val format = Json.valueFormat[ICDO3M]
  implicit val system = Coding.System[ICDO3M]("ICD-O-3-M")

  import scala.language.implicitConversions

  implicit def toString(icdo3t: ICD10GM): String = icdo3t.value
}


final case class TumorMorphology
(
  id: TumorMorphology.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  value: Coding[ICDO3M],
  note: Option[String]
)

object TumorMorphology
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]
  implicit val format   = Json.format[TumorMorphology]
}



final case class HistologyReport
(
  id: HistologyReport.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  issuedOn: Option[LocalDate],
  tumorMorphology: Option[TumorMorphology],
  tumorCellContent: Option[TumorCellContent],
)

object HistologyReport
{
  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]
  implicit val format   = Json.format[HistologyReport]
}

