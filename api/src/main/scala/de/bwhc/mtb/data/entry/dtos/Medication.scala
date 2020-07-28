package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json



case class Medication(value: String) extends AnyVal

object Medication
{
  implicit val format = Json.valueFormat[Medication]

  implicit val system = Coding.System[Medication]("ATC")
}
