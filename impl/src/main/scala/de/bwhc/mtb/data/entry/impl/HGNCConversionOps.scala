package de.bwhc.mtb.data.entry.impl


import de.bwhc.catalogs.hgnc.{HGNCCatalog,HGNCGene}

import de.bwhc.mtb.data.entry.dtos.{Coding,Variant}


trait HGNCConversionOps
{

  import cats.Id

  import Variant._


  val hgnc: HGNCCatalog[Id]


  final def codingOf(hgncId: HgncId): Option[Coding[Gene]] =
    hgnc.gene(HGNCGene.Id(hgncId.value))
      .map(
        gene =>
          Coding(
            Gene(gene.symbol),
            Some(gene.name)
          )
      )

  final def codingOf(hgncId: Coding[HgncId]): Option[Coding[Gene]] =
    codingOf(hgncId.code)


  final def resolve(coding: Coding[Gene]): Option[(Coding[HgncId],Coding[Gene])] = {

    val symbol = coding.code.value   
 
    val genes = hgnc.geneWithSymbol(symbol)

    genes.find(_.symbol equalsIgnoreCase symbol)
      .orElse(
        Option(
          genes.filter(_.aliasSymbols.exists(_ equalsIgnoreCase symbol))
        )
        .filter(_.size == 1)
        .map(_.head)
      )
      .orElse(
        Option(
          genes.filter(_.previousSymbols.exists(_ equalsIgnoreCase symbol))
        )
        .filter(_.size == 1)
        .map(_.head)
      )
      .map(   
        gene => 
          (
            Coding(
              HgncId(gene.id.value),
              None
            ),
            Coding(
              Gene(gene.symbol),
              Some(gene.name)
            )
          )
      )
  }

/*
  final def resolve(coding: Coding[Gene]): Option[(HgncId,Coding[Gene])] =
    hgnc.geneWithSymbol(coding.code.value) match {

      // Resolution only possible if the gene symbol is non-ambiguous,
      // i.e. the List of matches contains 1 element
      case gene :: Nil =>
        Some(
          (
            HgncId(gene.id.value),
            Coding(
              Gene(gene.symbol),
              Some(gene.name)
            )
          )
        )

      case _ => None

    }
*/

}
object HGNCConversionOps extends HGNCConversionOps
{
  override val hgnc =
    HGNCCatalog.getInstance.get
}


/*
trait HGNCConversionOps[F[_]]
{
  import cats.Applicative
  import cats.syntax.functor._


  def codingOf(
    hgncId: HgncId
  )(
    implicit
    hgnc: HGNCCatalog[F],
    F: Applicative[F]
  ): F[Option[Coding[Gene]]] =
    hgnc.gene(HGNCGene.Id(hgncId.value))
      .map(
        opt => opt.map( gene =>
          Coding(
            Gene(gene.symbol),
            Some(gene.name)
          )
        )
      )


  def resolve(
    coding: Coding[Gene]
  )(
    implicit
    hgnc: HGNCCatalog[F],
    F: Applicative[F]
  ): F[Option[(HgncId,Coding[Gene])]] = {

    hgnc.geneWithSymbol(coding.code.value).map {

      // Resolution only possible if the gene symbol is non-ambiguous,
      // i.e. the List of matches contains 1 element
      case gene :: Nil =>
        Some(
          (
            HgncId(gene.id.value),
            Coding(
              Gene(gene.symbol),
              Some(gene.name)
            )
          )
        )

      case _ => None

    }

  }

}
object HGNCConversionOps extends HGNCConversionOps[cats.Id]
*/

