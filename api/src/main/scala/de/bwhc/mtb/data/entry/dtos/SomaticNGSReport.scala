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
  sequencingType: SomaticNGSReport.SequencingType.Value,
  metadata: List[SomaticNGSReport.MetaData],
//  metadata: SomaticNGSReport.MetaData,
  tumorCellContent: TumorCellContent,
  brcaness: Option[BRCAness],
  msi: Option[MSI],
  tmb: TMB,
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


object ReferenceGenome extends Enumeration
{
  val HG19, HG38 = Value
  
  implicit val format = Json.formatEnum(this)
}


object SomaticNGSReport
{

  case class Id(value: String) extends AnyVal

  object SequencingType extends Enumeration
  {
    val TargetedNGS = Value("tNGS")
    val WGS         = Value("WGS")
    val WES         = Value("WES")

    implicit val format = Json.formatEnum(this)
  }


  final case class MetaData
  (
    kitType: String,
    kitManufacturer: String,
    sequencer: String,
    referenceGenome: ReferenceGenome.Value,
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
