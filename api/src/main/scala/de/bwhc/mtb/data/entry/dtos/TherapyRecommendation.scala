package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

//import cats.data.NonEmptyList

import play.api.libs.json.Json

import de.bwhc.util.json._



case class LevelOfEvidence
(
  grading: Coding[LevelOfEvidence.Grading.Value],
  addendums: Option[Set[Coding[LevelOfEvidence.Addendum.Value]]]
)

object LevelOfEvidence
{
 
  object Grading extends Enumeration
  {
    val m1A,m1B,m1C,m2A,m2B,m2C,m3,m4 = Value

    implicit val format = Json.formatEnum(this)

    implicit val system = Coding.System[Grading.Value]("MTB-CDS:Level-of-Evidence:Grading")
  } 
  
  object Addendum extends Enumeration
  {
    val IS = Value("is")
    val IV = Value("iv")
    val Z  = Value("Z")
    val R  = Value("R")

    implicit val format = Json.formatEnum(this)
    
    implicit val system = Coding.System[Addendum.Value]("MTB-CDS:Level-of-Evidence:Addendum")
  }

  implicit val format = Json.format[LevelOfEvidence]
}


case class TherapyRecommendation
(
  id: TherapyRecommendation.Id,
  patient: Patient.Id,
  diagnosis: Diagnosis.Id,
  issuedOn: Option[LocalDate],
  medication: Option[List[Coding[Medication]]],
//  medication: NonEmptyList[Coding[Medication]],
  priority: Option[TherapyRecommendation.Priority.Value],
  levelOfEvidence: Option[LevelOfEvidence],
//  ngsReport: SomaticNGSReport.Id,
  ngsReport: Option[SomaticNGSReport.Id],
  supportingVariants: Option[List[Variant.Id]]
)


object TherapyRecommendation
{

  case class Id(value: String) extends AnyVal
    
  implicit val formatId = Json.valueFormat[Id]


  object Priority extends Enumeration
  {
    val One   = Value("1")
    val Two   = Value("2")
    val Three = Value("3")
    val Four  = Value("4")

    implicit val format = Json.formatEnum(this)
  }
 
  implicit val format = Json.format[TherapyRecommendation]
}

