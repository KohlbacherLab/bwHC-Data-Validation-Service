package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import play.api.libs.json.Json



final case class Specimen
(
  id: Specimen.Id,
  patient: Patient.Id,
  icd10: Coding[ICD10GM],
  `type`: Option[Specimen.Type.Value],
  collection: Option[Specimen.Collection]
)


object Specimen
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]


  object Type extends Enumeration
  {
    val FreshTissue  = Value("fresh-tissue")
    val CryoFrozen   = Value("cryo-frozen")
    val FFPE         = Value("FFPE")
    val LiquidBiopsy = Value("liquid-biopsy")
    val Unknown      = Value("unknown")

    implicit val format = Json.formatEnum(this)
    implicit val system = Coding.System[Value]("Proben-Art")
  }


  final case class Collection
  (
    date: LocalDate,
    localization: Collection.Localization.Value,
    method: Collection.Method.Value
  )

  object Collection
  {

    object Localization extends Enumeration
    {
      val PrimaryTumor = Value("primary-tumor")
      val Metastasis   = Value("metastasis")
      val Unknown      = Value("unknown")
 
      implicit val format = Json.formatEnum(this)
      implicit val system = Coding.System[Value]("Proben-Lokalisierung")
    }

    object Method extends Enumeration
    {
      val Biopsy       = Value("biopsy")
      val Resection    = Value("resection")
      val LiquidBiopsy = Value("liquid-biopsy")
      val Cytology     = Value("cytology")
      val Unknown      = Value("unknown")
    
      implicit val format = Json.formatEnum(this)
      implicit val system = Coding.System[Value]("Proben-Entnahme-Methode")
    }

    implicit val format = Json.format[Collection]
    
  }

  implicit val format = Json.format[Specimen]

}
