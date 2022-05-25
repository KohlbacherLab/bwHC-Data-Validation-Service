package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json



object Gender extends Enumeration
{

  type Gender = Value

  val Male    = Value("male")
  val Female  = Value("female")
  val Other   = Value("other")
  val Unknown = Value("unknown")

  implicit val format = Json.formatEnum(this)

  implicit val system = Coding.System[Gender.Value]("Geschlecht")

}

