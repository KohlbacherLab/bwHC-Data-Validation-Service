package de.bwhc.mtb.data.entry.views


import java.time.LocalDate

import play.api.libs.json.Json

import de.bwhc.util.json._

import de.bwhc.mtb.data.entry.dtos.{
  Patient,
  SomaticNGSReport,
  ReferenceGenome,
  Specimen,
  Variant,
  SimpleVariant,
  CNV,
  Chromosome,
  RNAFusion,
  RNASeq,
  Gene,
  Intergenic
}
import SomaticNGSReport._
import Variant._



case class BRCAnessDisplay(value: String) extends AnyVal
object BRCAnessDisplay
{
  implicit val format = Json.valueFormat[BRCAnessDisplay]
}

case class TMBDisplay(value: String) extends AnyVal
object TMBDisplay
{
  implicit val format = Json.valueFormat[TMBDisplay]
}


case class GeneDisplay(value: String) extends AnyVal
object GeneDisplay
{
  implicit val format = Json.valueFormat[GeneDisplay]
}

case class StartEndDisplay(value: String) extends AnyVal
object StartEndDisplay
{
  implicit val format = Json.valueFormat[StartEndDisplay]
}



final case class SimpleVariantView
(
  patient: Option[Patient.Id],
  chromosome: Chromosome,
  gene: NotAvailable Or GeneDisplay,
  startEnd: StartEndDisplay,
  refAllele: Allele,
  altAllele: Allele,
  dnaChange: NoValue Or SimpleVariant.DNAChange,
  aminoAcidChange: NoValue Or SimpleVariant.AminoAcidChange,
  readDepth: AllelicReadDepth,
  allelicFrequency: AllelicFrequency,
  cosmicId: NotAvailable Or CosmicId,
  dbSNPId: NotAvailable Or SimpleVariant.DbSNPId,
  interpretation: Interpretation
)
object SimpleVariantView
{
  implicit val format = Json.writes[SimpleVariantView]
}


final case class CNVView
(
  patient: Option[Patient.Id],
  chromosome: Chromosome,
  reportedAffectedGenes: NotAvailable Or GeneDisplay,
  startRange: StartEndDisplay,
  endRange: StartEndDisplay,
  totalCopyNumber: NotAvailable Or Int,
  relativeCopyNumber: NotAvailable Or Double,
  cnA: NotAvailable Or Double,
  cnB: NotAvailable Or Double,
  reportedFocality: NotAvailable Or String,
  `type`: CNV.Type.Value,
  copyNumberNeutralLoH: NotAvailable Or GeneDisplay,
)
object CNVView
{
  implicit val format = Json.writes[CNVView]
}


final case class DNAFusionView
(
  patient: Option[Patient.Id],
  representation: String,
  reportedNumReads: NotAvailable Or Int
)
object DNAFusionView
{
  implicit val format = Json.writes[DNAFusionView]
}


final case class RNAFusionView
(
  patient: Option[Patient.Id],
  representation: String,
  position5pr: Intergenic Or RNAFusion.TranscriptPosition,
  strand5pr: Intergenic Or RNAFusion.Strand.Value,
  position3pr: Intergenic Or RNAFusion.TranscriptPosition,
  strand3pr: Intergenic Or RNAFusion.Strand.Value,
  effect: NotAvailable Or RNAFusion.Effect,
  cosmicId: NotAvailable Or CosmicId,
  reportedNumReads: NotAvailable Or Int
)
object RNAFusionView
{
  implicit val format = Json.writes[RNAFusionView]
}


final case class RNASeqView
(
  patient: Option[Patient.Id],
  entrezId: Gene.EntrezId,
  ensemblId: Gene.EnsemblId,
  gene: NotAvailable Or Gene.Symbol,
  transcriptId: TranscriptId,
  fragmentsPerKilobaseMillion: Double,
  fromNGS: Boolean,
  tissueCorrectedExpression: Boolean,
  rawCounts: Int,
  librarySize: Int,
  cohortRanking: NotAvailable Or Int
)
object RNASeqView
{
  implicit val format = Json.writes[RNASeqView]
}




final case class NGSReportView
(
  id: SomaticNGSReport.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  issueDate: LocalDate,
  sequencingType: SomaticNGSReport.SequencingType,
  metadata: List[SomaticNGSReport.MetaData],
  tumorCellContent: NotAvailable Or TumorCellContentDisplay,
  brcaness: NotAvailable Or BRCAnessDisplay,
//  brcaness: NotAvailable Or BRCAness,
  microSatelliteInstabilities: NotAvailable Or MSI,
  tumorMutationalBurden: NotAvailable Or TMBDisplay,
  simpleVariants: List[SimpleVariantView],
  copyNumberVariants: List[CNVView],
  dnaFusions: List[DNAFusionView],
  rnaFusions: List[RNAFusionView],
  rnaSeqs: List[RNASeqView]
)

object NGSReportView
{
  implicit val format = Json.writes[NGSReportView]
}

