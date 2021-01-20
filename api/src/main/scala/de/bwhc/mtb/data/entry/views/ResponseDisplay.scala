package de.bwhc.mtb.data.entry.views


import play.api.libs.json.Json

import de.bwhc.mtb.data.entry.dtos.RECIST


case class ResponseDisplay(value: String) extends AnyVal
object ResponseDisplay
{
  implicit val format = Json.valueFormat[ResponseDisplay]
}
