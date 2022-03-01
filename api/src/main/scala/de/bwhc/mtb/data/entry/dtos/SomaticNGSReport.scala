package de.bwhc.mtb.data.entry.dtos


import java.net.URI

import java.time.LocalDate

import play.api.libs.json.Json


import SomaticNGSReport._


case class SomaticNGSReport
(
  id: SomaticNGSReport.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  issueDate: LocalDate,
  sequencingType: SomaticNGSReport.SequencingType,
  metadata: List[SomaticNGSReport.MetaData],
  tumorCellContent: Option[TumorCellContent],
  brcaness: Option[BRCAness],
  msi: Option[MSI],
  tmb: Option[TMB],
  simpleVariants: Option[List[SimpleVariant]],
  copyNumberVariants: Option[List[CNV]],
  dnaFusions: Option[List[DNAFusion]],
  rnaFusions: Option[List[RNAFusion]],
  rnaSeqs: Option[List[RNASeq]]
)
{
  def variants: List[Variant] =
    simpleVariants.getOrElse(List.empty[Variant]) :++
    copyNumberVariants.getOrElse(List.empty[Variant]) :++
    dnaFusions.getOrElse(List.empty[Variant]) :++
    rnaFusions.getOrElse(List.empty[Variant]) :++
    rnaSeqs.getOrElse(List.empty[Variant])
}


case class ReferenceGenome(value: String) extends AnyVal
object ReferenceGenome 
{
  implicit val format = Json.valueFormat[ReferenceGenome]
}


object SomaticNGSReport
{

  case class Id(value: String) extends AnyVal


  case class SequencingType(value: String) extends AnyVal
  object SequencingType 
  {
    implicit val format = Json.valueFormat[SequencingType]
  }



  final case class MetaData
  (
    kitType: String,
    kitManufacturer: String,
    sequencer: String,
    referenceGenome: ReferenceGenome,
    pipeline: Option[URI]
  )



  case class MSI(value: Double) extends AnyVal // Micro-Satellite Instabilities
  case class TMB(value: Double) extends AnyVal // Tumor Mutational Burden
  case class BRCAness(value: Double) extends AnyVal
  case class AverageReadDepth(value: Double) extends AnyVal

  implicit val formatId          = Json.valueFormat[Id]
  implicit val formatTMB         = Json.valueFormat[TMB]
  implicit val formatMSI         = Json.valueFormat[MSI]
  implicit val formatBRCAness    = Json.valueFormat[BRCAness]
  implicit val formatAvgReadDpth = Json.valueFormat[AverageReadDepth]
  implicit val formatMetaData    = Json.format[MetaData]


  implicit val format = Json.format[SomaticNGSReport]
}
