package de.bwhc.mtb.data.entry.dtos


import play.api.libs.json.Json


case class Gene(symbol: String) extends AnyVal
object Gene
{
  implicit val format = Json.valueFormat[Gene]
  implicit val system = Coding.System[Gene]("HGNC")
}


case class Chromosome(number: Int) extends AnyVal
object Chromosome
{
  implicit val format = Json.valueFormat[Chromosome]
}


object Variant
{

  object Type extends Enumeration
  {
    val Simple, CNV = Value
  }

  case class CosmicId(value: String) extends AnyVal
  case class Interpretation(value: String) extends AnyVal
  case class FunctionalAnnotation(value: String) extends AnyVal
  case class Allele(value: String) extends AnyVal
  case class AllelicReadDepth(value: Int) extends AnyVal
  case class AllelicFrequency(value: Double) extends AnyVal

  case class StartEnd
  (
    chromosome: Chromosome,
    start: Int,
    end: Int
  ){
    override def toString =
      s"${chromosome.number}:$start-$end"
  }


  implicit val formatCosmicId = Json.valueFormat[CosmicId]

}


import Variant._

sealed trait SomaticVariant
{
//  val typeOf: Type
  val gene: Coding[Gene]
  val cosmicId: CosmicId
  val interpretation: Interpretation
}

/*
object SimpleVariant
{
  case class AminoAcidChange(value: String)
  case class DNAChange(value: String)
  case class DbSNPId(value: String) extends Identifier("dbSNPId")
}

case class SimpleVariant
(
  gene: HGNCGene.Symbol,
  startEnd: StartEnd,
  refAllele: Allele,
  altAllele: Allele,
  dnaChange: SimpleVariant.DNAChange,
  aminoAcidChange: SimpleVariant.AminoAcidChange,
  readDepth: AllelicReadDepth,
  allelicFrequency: AllelicFrequency,
  cosmicId: CosmicId,
  dbSNPId: SimpleVariant.DbSNPId,
  interpretation: Interpretation
) extends SomaticVariant
*/
