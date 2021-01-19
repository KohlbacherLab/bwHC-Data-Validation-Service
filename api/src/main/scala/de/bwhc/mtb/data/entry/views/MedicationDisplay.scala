package de.bwhc.mtb.data.entry.views



import play.api.libs.json.Json


import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Medication
}


final case class MedicationDisplay(value: String) extends View[Coding[Medication]]

object MedicationDisplay
{

  implicit val format = Json.format[MedicationDisplay]

  implicit val dflt = Default(MedicationDisplay("-"))

}

