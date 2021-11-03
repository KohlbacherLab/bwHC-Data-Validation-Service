package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json

/*
case class Medication(value: String) extends AnyVal

object Medication
{
  implicit val format = Json.valueFormat[Medication]

  implicit val system = Coding.System[Medication]("ATC")
}
*/


object Medication
{

  case class Code(value: String) extends AnyVal

  implicit val format = Json.valueFormat[Code]


  object System extends Enumeration
  {
    val ATC          = Value
    val Unregistered = Value

    implicit val format = Json.formatEnum(this)
  }

  case class Coding
  (
    code: Code,
    system: System.Value,
    display: Option[String],
    version: Option[String]
  )

  object Coding {
    implicit val format = Json.format[Coding]
  }

}

