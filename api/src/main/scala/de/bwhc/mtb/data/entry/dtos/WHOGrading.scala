package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json


object WHOGrade extends Enumeration
{
  val I   = Value("I")
  val II  = Value("II")
  val III = Value("III")
  val IV  = Value("IV")

  implicit val format = Json.formatEnum(this)

  implicit val system = Coding.System[WHOGrade.Value]("WHO-Grading-CNS-Tumors")
}

