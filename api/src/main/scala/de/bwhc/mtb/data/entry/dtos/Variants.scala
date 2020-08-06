package de.bwhc.mtb.data.entry.dtos



import play.api.libs.json.Json



case class Chromosome private (number: Int) extends AnyVal
object Chromosome
{

  val instances: List[Chromosome] =
    (1 to 23).map(new Chromosome(_)).toList
  
  def apply(num: Int): Chromosome =
    instances(num-1)

  implicit val format = Json.valueFormat[Chromosome]
  
}


object Variant
{

  object Type extends Enumeration
  {
    val Simple, CNV = Value

    implicit val format = Json.formatEnum(this)
  }

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
    implicit val system = Coding.System[FunctionalAnnotation]("TODO: System FunctionalAnnotation")
  }


  case class Allele(value: String) extends AnyVal
  implicit val formatAllele = Json.valueFormat[Allele]


  case class AllelicFrequency(value: Double) extends AnyVal
  implicit val formatAllelicFrequency = Json.valueFormat[AllelicFrequency]


  case class AllelicReadDepth(value: Int) extends AnyVal
  implicit val formatAllelicReadDepth = Json.valueFormat[AllelicReadDepth]


  final case class StartEnd(
    start: Long,
    end: Option[Long] = None,
  )
  object StartEnd
  {
    implicit val format = Json.format[StartEnd]
  }


}


import Variant._

sealed trait SomaticVariant
{
  val gene: Coding[Gene]
  val cosmicId: CosmicId
  val interpretation: Coding[Interpretation]
}


case class SimpleVariant
(
  chromosome: Chromosome,
  gene: Coding[Gene],
  startEnd: StartEnd,
  refAllele: Allele,
  altAllele: Allele,
  dnaChange: Coding[SimpleVariant.DNAChange],
  aminoAcidChange: Coding[SimpleVariant.AminoAcidChange],
  readDepth: AllelicReadDepth,
  allelicFrequency: AllelicFrequency,
  cosmicId: CosmicId,
  dbSNPId: Coding[SimpleVariant.DbSNPId],
  interpretation: Coding[Interpretation]
) extends SomaticVariant


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
