package de.bwhc.mtb.data.entry.dtos



import java.time.LocalDate

import play.api.libs.json.Json




final case class Claim
(
  id: Claim.Id,
  patient: Patient.Id,
  issuedOn: LocalDate,
  therapy: TherapyRecommendation.Id
)

object Claim
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

  implicit val format = Json.format[Claim]
}



final case class ClaimResponse
(
  id: ClaimResponse.Id,
  issuedOn: LocalDate,
  claim: Claim.Id,
  patient: Patient.Id,
  status: ClaimResponse.Status.Value,
  reason: ClaimResponse.Reason.Value 
)

object ClaimResponse
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]


  object Status extends Enumeration
  {
    val Accepted = Value("accepted")
    val Rejected = Value("rejected")

    implicit val format = Json.formatEnum(this)
  }


  object Reason extends Enumeration
  {
    val InsufficientEvidence        = Value("insufficient-evidence")
    val StandardTherapyNotExhausted = Value("standard-therapy-not-exhausted")
    val Other                       = Value("other")

    implicit val format = Json.formatEnum(this)
  }



  implicit val format = Json.format[ClaimResponse]
}
