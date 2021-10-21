package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json

import cats.data.NonEmptyList

import de.bwhc.util.data.Interval



case class Chromosome private (value: String) extends AnyVal
object Chromosome
{

  val instances: List[Chromosome] =
    ((1 to 22)
      .map(_.toString)
      .toList :+ "X" :+ "Y")
      .map(s => s"chr$s")
      .map(Chromosome(_))
  
  implicit val format = Json.valueFormat[Chromosome]
  
}


sealed abstract class Variant
{
  val id: Variant.Id
}


object Variant
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]


  case class Gene(value: String) extends AnyVal
  object Gene
  {
    implicit val format = Json.valueFormat[Gene]
    implicit val system = Coding.System[Gene]("HGNC")
  }

  case class CosmicId(value: String) extends AnyVal
  object CosmicId
  {
    implicit val format = Json.valueFormat[CosmicId]
  }


  case class TranscriptId(value: String) extends AnyVal
  object TranscriptId
  {
    implicit val format = Json.valueFormat[TranscriptId]
  }


  case class Interpretation(value: String) extends AnyVal
  object Interpretation
  {
    implicit val format = Json.valueFormat[Interpretation]
    implicit val system = Coding.System[Interpretation]("ClinVAR")
  }
  

  case class FunctionalAnnotation(value: String) extends AnyVal
  object FunctionalAnnotation
  {
    implicit val format = Json.valueFormat[FunctionalAnnotation]
    implicit val system = Coding.System[FunctionalAnnotation]("SequenceOntology")
  }


  case class Allele(value: String) extends AnyVal
  implicit val formatAllele = Json.valueFormat[Allele]


  case class AllelicFrequency(value: Double) extends AnyVal
  implicit val formatAllelicFrequency = Json.valueFormat[AllelicFrequency]


  case class AllelicReadDepth(value: Int) extends AnyVal
  implicit val formatAllelicReadDepth = Json.valueFormat[AllelicReadDepth]


  final case class StartEnd(start: Long, end: Option[Long])
  object StartEnd
  {
    implicit val format = Json.format[StartEnd]
  }


}


import Variant._

case class SimpleVariant
(
  id: Variant.Id,
  chromosome: Chromosome,
  gene: Option[Coding[Gene]],
  startEnd: StartEnd,
  refAllele: Allele,
  altAllele: Allele,
  functionalAnnotation: Option[Coding[FunctionalAnnotation]],
  dnaChange: Option[Coding[SimpleVariant.DNAChange]],
  aminoAcidChange: Option[Coding[SimpleVariant.AminoAcidChange]],
  readDepth: AllelicReadDepth,
  allelicFrequency: AllelicFrequency,
  cosmicId: Option[CosmicId],
  dbSNPId: Option[SimpleVariant.DbSNPId],
  interpretation: Coding[Interpretation]
)
extends Variant


object SimpleVariant
{

  case class DbSNPId(value: String) extends AnyVal
  object DbSNPId
  {
    implicit val format = Json.valueFormat[DbSNPId]
    implicit val system = Coding.System[DbSNPId]("dbSNP")
  }

  case class AminoAcidChange(value: String) extends AnyVal
  object AminoAcidChange
  {
    implicit val format = Json.valueFormat[AminoAcidChange]
    implicit val system = Coding.System[AminoAcidChange]("HGVS.p")
  }

  case class DNAChange(value: String) extends AnyVal
  object DNAChange
  {
    implicit val format = Json.valueFormat[DNAChange]
    implicit val system = Coding.System[DNAChange]("HGVS.c")
  }

  implicit val format = Json.format[SimpleVariant]

}


final case class CNV
(
  id: Variant.Id,
  chromosome: Chromosome,
  startRange: StartEnd,
  endRange: StartEnd,
  totalCopyNumber: Int,
  relativeCopyNumber: Double,
  cnA: Option[Double],
  cnB: Option[Double],
  reportedAffectedGenes: Option[List[Coding[Gene]]],
  reportedFocality: Option[String],
  `type`: CNV.Type.Value,
  copyNumberNeutralLoH: Option[List[Coding[Gene]]],
)
extends Variant

object CNV
{

  object Type extends Enumeration
  {
    val LowLevelGain  = Value("low-level-gain")
    val HighLevelGain = Value("high-level-gain")
    val Loss          = Value("loss")

    implicit val format = Json.formatEnum(this)
  }

  implicit val format = Json.format[CNV]
}



final case class DNAFusion
(
  id: Variant.Id,
  domain5prime: DNAFusion.FunctionalDomain,
  domain3prime: DNAFusion.FunctionalDomain,
  reportedNumReads: Int
)
extends Variant

object DNAFusion
{

  final case class FunctionalDomain
  (
    chromosome: Chromosome,
    position: Long,
    gene: Coding[Gene]
  )

  implicit val formatDomain = Json.format[FunctionalDomain]

  implicit val format = Json.format[DNAFusion]
}



final case class RNAFusion
(
  id: Variant.Id,
  domain5prime: RNAFusion.FunctionalDomain,
  domain3prime: RNAFusion.FunctionalDomain,
  effect: Option[RNAFusion.Effect],
  cosmicId: Option[CosmicId],
  reportedNumReads: Int
)
extends Variant

object RNAFusion
{


  case class TranscriptPosition(value: Long) extends AnyVal

  case class ExonId(value: String) extends AnyVal


  object Strand extends Enumeration
  {
    val Plus  = Value("+")
    val Minus = Value("-")

    implicit val format = Json.formatEnum(this)
  }


  final case class FunctionalDomain
  (
    gene: Coding[Gene],
    transcriptId: TranscriptId,
    exon: ExonId,
    position: TranscriptPosition,
    strand: Strand.Value
  )


  case class Effect(value: String) extends AnyVal


  implicit val formatEffect        = Json.valueFormat[Effect]
  implicit val formatTranscriptPos = Json.valueFormat[TranscriptPosition]
  implicit val formatExonId        = Json.valueFormat[ExonId]

  implicit val formatDomain = Json.format[FunctionalDomain]

  implicit val format = Json.format[RNAFusion]
}




final case class RNASeq
(
  id: Variant.Id,
  entrezId: RNASeq.EntrezId,
  ensemblId: RNASeq.EnsemblId,
  gene: Coding[Gene],
  transcriptId: TranscriptId,
  fragmentsPerKilobaseMillion: Double,
  fromNGS: Boolean,
  tissueCorrectedExpression: Boolean,
  rawCounts: Int,
  librarySize: Int,
  cohortRanking: Option[Int]
)
extends Variant


object RNASeq
{

  case class EntrezId(value: String) extends AnyVal
  
  case class EnsemblId(value: String) extends AnyVal

  implicit val formatEntrezId       = Json.valueFormat[EntrezId]
  implicit val formatEnsemblId      = Json.valueFormat[EnsemblId]

  implicit val format = Json.format[RNASeq]
}

