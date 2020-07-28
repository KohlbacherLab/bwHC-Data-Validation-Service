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
}

/*
object GenderValueSets
{

  val valueSetEN: ValueSet[Gender.Value] =
    ValueSet(
      "AdministrativeGender",
      List(
        ValueSet.Concept(Gender.Male   ,"Male"),
        ValueSet.Concept(Gender.Female ,"Female"),
        ValueSet.Concept(Gender.Other  ,"Other"),
        ValueSet.Concept(Gender.Unknown,"Unknown")
      )
    )

  val valueSetDE: ValueSet[Gender.Value] =
    ValueSet(
      "Geschlecht",
      List(
        ValueSet.Concept(Gender.Male   ,"Männlich"),
        ValueSet.Concept(Gender.Female ,"Weiblich"),
        ValueSet.Concept(Gender.Other  ,"Sonstiges"),
        ValueSet.Concept(Gender.Unknown,"Unbekannt")
      )
    )

  val valueSets: Map[Language.Value,ValueSet[Gender.Value]] =
    Map(
      Language.EN -> valueSetEN,
      Language.DE -> valueSetDE
    )

}
*/
