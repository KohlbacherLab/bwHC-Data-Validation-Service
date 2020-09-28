package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json



case class TumorCellContent
(
  specimen: Specimen.Id, 
  method: TumorCellContent.Method.Value,
  value: Double
)


object TumorCellContent
{

  object Method extends Enumeration
  {
    val Histologic    = Value("histologic")
    val Bioinformatic = Value("bioinformatic")

    implicit val format = Json.formatEnum(this)
  }

  implicit val format = Json.format[TumorCellContent]

}
