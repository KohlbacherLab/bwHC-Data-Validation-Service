package de.bwhc.mtb.data.entry.impl


import de.bwhc.catalogs.hgnc.{HGNCCatalog,HGNCGene}

import de.bwhc.mtb.data.entry.dtos.{Coding,Variant}


trait HGNCConversionOps
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



  final def resolve(coding: Coding[GeneSymbol]): Option[(Coding[HgncId],Coding[GeneSymbol])] = {

    val symbol = coding.code.value   
 
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

  final def resolve(symbol: GeneSymbol): Option[(Coding[HgncId],Coding[GeneSymbol])] =
    resolve(Coding(symbol,None))

/*

  final def resolve(coding: Coding[GeneSymbol]): Option[(Coding[HgncId],Coding[GeneSymbol])] = {

    val symbol = coding.code.value   
 
    val genes = hgnc.geneWithSymbol(symbol)

    // 1. Try looking up the gene by approved symbol...
    genes.find(_.symbol equalsIgnoreCase symbol)
      .filter(
        // ... and ensure that the approved symbol is not an alias or previous symbol as well 
//        hit => !(genes exists (g => (g.aliasSymbols ++ g.previousSymbols).exists(_ equalsIgnoreCase symbol)))
        hit => !(genes exists (g => (g.aliasSymbols ++ g.previousSymbols) contains symbol))
      )
      .orElse(
        // 2. else try whether it can be resolved UNIQUELY by alias symbol 
        Option(
          genes.filter(_.aliasSymbols.exists(_ equalsIgnoreCase symbol))
        )
        .filter(_.size == 1)  
        .map(_.head)
        .filter(
          // ... and hat the approved symbol is not an alias symbol as well 
//          hit => !(genes exists (_.previousSymbols.exists(_ equalsIgnoreCase symbol)))
          hit => !(genes exists (_.previousSymbols contains symbol))
        )
      )
      .orElse(
        // 3. else try whether it can be resolved UNIQUELY by previous symbol 
        Option(
          genes.filter(_.previousSymbols.exists(_ equalsIgnoreCase symbol))
        )
        .filter(_.size == 1)
        .map(_.head)
        .filter(
          // ... and ensure that the approved symbol is not an alias symbol as well 
//          hit => !(genes exists (_.aliasSymbols.exists(_ equalsIgnoreCase symbol)))
          hit => !(genes exists (_.aliasSymbols contains symbol))
        )
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
*/

}

object HGNCConversionOps extends HGNCConversionOps
{
  override val hgnc =
    HGNCCatalog.getInstance.get
}


