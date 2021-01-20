package de.bwhc.mtb.data.entry.views



import play.api.libs.json.Json


import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Medication
}


final case class MedicationDisplay(value: String) extends AnyVal

object MedicationDisplay
{

  implicit val format = Json.valueFormat[MedicationDisplay]

  implicit val dflt = Default(MedicationDisplay("-"))

}

