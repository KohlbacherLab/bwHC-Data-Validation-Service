package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.{
  Json,JsError,JsString,JsSuccess,
  Format,Reads,Writes
}
import cats.data.NonEmptyList



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


object Gene
{

  case class HgncId(value: String) extends AnyVal

  implicit val hgncSystem = de.bwhc.mtb.data.entry.dtos.Coding.System[HgncId]("HGNC")


  case class EntrezId(value: String) extends AnyVal
  
  case class EnsemblId(value: String) extends AnyVal

  case class Symbol(value: String) extends AnyVal

  implicit val formatHgncId       = Json.valueFormat[HgncId]
  implicit val formatEntrezId     = Json.valueFormat[EntrezId]
  implicit val formatEnsemblId    = Json.valueFormat[EnsemblId]
  implicit val formatSymbol       = Json.valueFormat[Symbol]


  final case class Coding
  (
    ensemblId: Option[EnsemblId],
    hgncId: Option[HgncId],
    symbol: Option[Symbol],
    name: Option[String]
  )
  object Coding
  {
    implicit val format = Json.format[Coding]
  }
}



sealed abstract class Variant
{
  val id: Variant.Id
  val patient: Option[Patient.Id]
}

object Variant
{

  case class Id(value: String) extends AnyVal

  implicit val formatId = Json.valueFormat[Id]

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
  patient: Option[Patient.Id],
  chromosome: Chromosome,
  gene: Option[Gene.Coding],
  startEnd: StartEnd,
  refAllele: Allele,
  altAllele: Allele,
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
  patient: Option[Patient.Id],
  chromosome: Chromosome,
  startRange: StartEnd,
  endRange: StartEnd,
  totalCopyNumber: Option[Int],
  relativeCopyNumber: Option[Double],
  cnA: Option[Double],
  cnB: Option[Double],
  reportedAffectedGenes: Option[List[Gene.Coding]],
  reportedFocality: Option[String],
  `type`: CNV.Type.Value,
  copyNumberNeutralLoH: Option[List[Gene.Coding]],
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



sealed trait Intergenic
final case object Intergenic extends Intergenic
{
  override def toString = "intergenic"

  implicit val format = 
    Format[Intergenic](
      Reads(
        js => js.validate[String].flatMap {
          case "intergenic" => JsSuccess(Intergenic)
          case x            => JsError("Expected value 'intergenic'")
        }
      ),
      Writes(ig => JsString("intergenic"))
    )
}


final case class DNAFusion
(
  id: Variant.Id,
  patient: Option[Patient.Id],
  fusionPartner5prime: Option[DNAFusion.Partner],
  fusionPartner3prime: Option[DNAFusion.Partner],
  reportedNumReads: Option[Int]
)
extends Variant

object DNAFusion
{

  final case class Partner
  (
    chromosome: Chromosome,
    position: Long,
    gene: Gene.Coding
  )

  implicit val formatPartner = Json.format[Partner]

  implicit val format = Json.format[DNAFusion]
}



final case class RNAFusion
(
  id: Variant.Id,
  patient: Option[Patient.Id],
  fusionPartner5prime: Option[RNAFusion.Partner],
  fusionPartner3prime: Option[RNAFusion.Partner],
  effect: Option[RNAFusion.Effect],
  cosmicId: Option[CosmicId],
  reportedNumReads: Option[Int]
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


  final case class Partner
  (
    gene: Gene.Coding,
    transcriptId: TranscriptId,
    exon: ExonId,
    position: TranscriptPosition,
    strand: Strand.Value
  )


  case class Effect(value: String) extends AnyVal


  implicit val formatEffect        = Json.valueFormat[Effect]
  implicit val formatTranscriptPos = Json.valueFormat[TranscriptPosition]
  implicit val formatExonId        = Json.valueFormat[ExonId]

  implicit val formatPartner = Json.format[Partner]

  implicit val format = Json.format[RNAFusion]
}




final case class RNASeq
(
  id: Variant.Id,
  patient: Option[Patient.Id],
  entrezId: Gene.EntrezId,
  ensemblId: Gene.EnsemblId,
  gene: Gene.Coding,
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
/*
  case class EntrezId(value: String) extends AnyVal
  
  case class EnsemblId(value: String) extends AnyVal

  implicit val formatEntrezId       = Json.valueFormat[EntrezId]
  implicit val formatEnsemblId      = Json.valueFormat[EnsemblId]
*/
  implicit val format = Json.format[RNASeq]
}

