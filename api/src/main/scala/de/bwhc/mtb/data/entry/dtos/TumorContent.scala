package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json



case class TumorContent
(
  specimen: Specimen.Id, 
  method: TumorContent.Method.Value,
  value: Double
)


object TumorContent
{

  object Method extends Enumeration
  {
    val Pathologic    = Value("pathologic")
    val Bioinformatic = Value("bioinformatic")

    implicit val format = Json.formatEnum(this)
  }

  implicit val format = Json.format[TumorContent]

}
