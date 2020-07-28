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
    val FreshTissue  = Value("Fresh Tissue")
    val CryoFrozen   = Value("Cryo-frozen")
    val FFPE         = Value("FFPE")
    val LiquidBiopsy = Value("Liquid Biopsy")
    val Unknown      = Value("Unknown")

    implicit val format = Json.formatEnum(this)
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
    }

    object Method extends Enumeration
    {
      val Biopsy       = Value("Biopsy")
      val Resection    = Value("Resection")
      val LiquidBiopsy = Value("Liquid Biopsy")
      val Cytology     = Value("Cytology")
      val Unknown      = Value("Unknown")
    
      implicit val format = Json.formatEnum(this)
    }

    implicit val format = Json.format[Collection]
    
  }

  implicit val format = Json.format[Specimen]

}
