package de.bwhc.mtb.data.entry.impl


import de.bwhc.catalogs.hgnc.{HGNCCatalog,HGNCGene,HGNCId}

import de.bwhc.mtb.data.entry.dtos.Gene

trait HGNCOps
{

  import cats.Id

  val hgnc: HGNCCatalog[Id]

  final def complete(coding: Gene.Coding): Option[Gene.Coding] = {

    import Gene._

    coding.ensemblId.flatMap(
      id => hgnc.geneWithEnsemblId(id.value)
    )
    .orElse(
      coding.hgncId.flatMap(id => hgnc.gene(HGNCId(id.value)))
    )
    .map(
      gene =>
        Gene.Coding(
          gene.ensemblId.map(id => EnsemblId(id.value)),
          Some(HgncId(gene.hgncId.value)),
          Some(Symbol(gene.symbol)),
          Some(gene.name)
        )
    )

  }

}

object HGNCOps extends HGNCOps
{
  override val hgnc =
    HGNCCatalog.getInstance.get
}


/*
trait HGNCOps
{

  import cats.Id

  import Variant.{HgncId,GeneSymbol}


  val hgnc: HGNCCatalog[Id]


  final def codingOf(hgncId: HgncId): Option[Coding[GeneSymbol]] =
    hgnc.gene(HGNCGene.Id(hgncId.value))
      .map(
        gene =>
          Coding(
            GeneSymbol(gene.symbol),
            Some(gene.name)
          )
      )

  final def codingOf(hgncId: Coding[HgncId]): Option[Coding[GeneSymbol]] =
    codingOf(hgncId.code)




  final def resolve(sym: GeneSymbol): Option[(Coding[HgncId],Coding[GeneSymbol])] = {

    val symbol = sym.value   
 
    val genes = hgnc.geneWithSymbol(symbol)

    lazy val aliasSymbolSet = genes.flatMap(_.aliasSymbols).toSet

    lazy val previousSymbolSet = genes.flatMap(_.previousSymbols).toSet


    // 1. Try looking up the gene by approved symbol...
    genes.find(_.symbol equalsIgnoreCase symbol)
      .filter(hit => !(aliasSymbolSet ++ previousSymbolSet).contains(symbol))
      .orElse(
        // 2. else try whether it can be resolved UNIQUELY as an alias symbol 
        Option(
          genes.filter(_.aliasSymbols.exists(_ equalsIgnoreCase symbol))
        )
        .filter(hits => hits.size == 1 && !previousSymbolSet.contains(symbol))
        .map(_.head)
        
      )
      .orElse(
        // 3. else try whether it can be resolved UNIQUELY as a previous symbol 
        Option(
          genes.filter(_.previousSymbols.exists(_ equalsIgnoreCase symbol))
        )
        .filter(hits => hits.size == 1 && !aliasSymbolSet.contains(symbol))
        .map(_.head)
      )
      .map(   
        gene => 
          (
            Coding(
              HgncId(gene.id.value),
              Some(gene.name)
            ),
            Coding(
              GeneSymbol(gene.symbol),
              Some(gene.name)
            )
          )
      )
  }

}

object HGNCOps extends HGNCOps
{
  override val hgnc =
    HGNCCatalog.getInstance.get
}
*/

