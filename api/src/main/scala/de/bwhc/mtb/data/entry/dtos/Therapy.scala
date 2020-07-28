package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json


case class TherapyId(value: String) extends AnyVal

object TherapyId
{
  implicit val format = Json.valueFormat[TherapyId]
}
