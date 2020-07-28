package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import cats.data.NonEmptyList

import play.api.libs.json.Json


object FamilyMember
{
  object Relationship extends Enumeration
  {
    type Relationship = Value
  
    val FamilyMember       = Value("FAMMEMB")
    val ExtendFamilyMember = Value("EXT")

    implicit val format    = Json.formatEnum(this)
  }
}


final case class FamilyMemberDiagnosis
(
  id: FamilyMemberDiagnosis.Id,
  patient: Patient.Id,
  relationship: FamilyMember.Relationship.Value  
)

object FamilyMemberDiagnosis
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[FamilyMemberDiagnosis]

}
