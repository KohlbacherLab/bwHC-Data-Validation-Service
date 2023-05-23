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
    .orElse(
      coding.symbol.flatMap(symbol => hgnc.geneWithApprovedSymbol(symbol.value))
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


