package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

import play.api.libs.json.Json


import SomaticNGSReport._


case class SomaticNGSReport
(
  id: SomaticNGSReport.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  issueDate: LocalDate,
  tumorContent: List[TumorContent],
  brcaness: BRCAness,
  msi: MSI,
  tmb: TMB,
//TODO: other QualityControl parameters
//  qualityControl: QualityControl,
  simpleVariants: List[SimpleVariant]
)


object SomaticNGSReport
{

  case class Id(value: String) extends AnyVal

  case class MSI(value: Double) extends AnyVal // Micro-Satellite Instabilities
  case class TMB(value: Double) extends AnyVal // Tumor Mutational Burden
  case class BRCAness(value: Double) extends AnyVal
  case class AverageReadDepth(value: Double) extends AnyVal

  implicit val formatId          = Json.valueFormat[Id]
  implicit val formatTMB         = Json.valueFormat[TMB]
  implicit val formatMSI         = Json.valueFormat[MSI]
  implicit val formatBRCAness    = Json.valueFormat[BRCAness]
  implicit val formatAvgReadDpth = Json.valueFormat[AverageReadDepth]

//  case class QualityControl(
//    avgReadDepth: AverageReadDepth
//  )


  implicit val format = Json.format[SomaticNGSReport]
}
