package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json



case class TumorCellContent
(
  id: TumorCellContent.Id, 
  specimen: Specimen.Id, 
  method: TumorCellContent.Method.Value,
  value: Double
)


object TumorCellContent
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]


  object Method extends Enumeration
  {
    val Histologic    = Value("histologic")
    val Bioinformatic = Value("bioinformatic")

    implicit val format = Json.formatEnum(this)
  }

  implicit val format = Json.format[TumorCellContent]

}
