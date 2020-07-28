package de.bwhc.mtb.data.entry.dtos


import play.api.libs.json.Json


object HealthInsurance
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]
}

