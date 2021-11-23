package de.bwhc.mtb.data.entry.impl


import org.scalatest.flatspec.AnyFlatSpec

import de.bwhc.catalogs.hgnc.{HGNCCatalog,HGNCGene}

import de.bwhc.mtb.data.entry.dtos.{Coding,Variant}



class HGNCConversionTests extends AnyFlatSpec 
{

  val hgnc = HGNCCatalog.getInstance.get

  import Variant._
  
/*
  "Resolving Codings by HGNC-IDs" must "have returned the expected approved gene symbols" in {

    assert(
      hgnc.genes
        .take(200)
        .forall {
          gene =>
            HGNCConversionOps.codingOf(HgncId(gene.id.value)) match {
              case Some(coding) => (coding.code.value == gene.symbol)

              case _ => false
            }
        }
    )
  }


  "Resolving HGNC-IDs and Codings by alias symbol" must
    "have returned unique hits with the expected HGNC-IDs and approved gene symbols" in {

    assert(
      hgnc.genes
        .filter(!_.aliasSymbols.isEmpty)
        .take(200)
        .forall { gene =>

          val alias = gene.aliasSymbols.head

          HGNCConversionOps.resolve(GeneSymbol(alias)) match {

            case Some(idCoding -> symbolCoding) => 
              idCoding.code.value == gene.id.value &&
                symbolCoding.code.value == gene.symbol

            case None => // If resolution has failed, then check that the symbol was indeed ambiguous
              hgnc.geneWithSymbol(alias).size > 1
            
          }
        }

    )
  }


  "Resolving HGNC-IDs and Codings by previous symbol" must
    "have returned unique hits with the expected HGNC-IDs and approved gene symbols" in {

    assert(
      hgnc.genes
        .filter(!_.previousSymbols.isEmpty)
        .take(200)
        .forall { gene =>

          val previous = gene.previousSymbols.head

          HGNCConversionOps.resolve(GeneSymbol(previous)) match {

            case Some(idCoding -> symbolCoding) =>
              idCoding.code.value == gene.id.value &&
                symbolCoding.code.value == gene.symbol

            // If resolution has failed, then check that the symbol was indeed ambiguous
            case None =>
              hgnc.geneWithSymbol(previous).size > 1

          }

        }

    )
  }
*/

}
