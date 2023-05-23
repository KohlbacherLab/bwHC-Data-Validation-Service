package de.bwhc.mtb.data.entry.impl

import de.bwhc.mtb.data.entry.dtos.Gene
import org.scalatest.funspec.AnyFunSpec

class HGNCOpsTest extends AnyFunSpec {

  private lazy val hgncOps = HGNCOps

  describe("Given gene symbol 'TP53'") {

    val coding = Gene.Coding.apply(
      hgncId = None,
      ensemblId = None,
      symbol = Some(Gene.Symbol("TP53")),
      name = None
    )


    it ("should complete gene coding with HGNC-ID 'HGNC:11998'") {

      assert(hgncOps.complete(coding).exists(
        _.hgncId.contains(Gene.HgncId("HGNC:11998"))
      ))

    }


    it("should complete gene coding with Ensembl-ID 'ENSG00000141510'") {

      assert(hgncOps.complete(coding).exists(
        _.ensemblId.contains(Gene.EnsemblId("ENSG00000141510"))
      ))

    }


    it("should complete gene coding with name 'tumor protein p53'") {

      assert(hgncOps.complete(coding).exists(
        _.name.contains("tumor protein p53"))
      )

    }

  }

  describe("Given gene HGNC-ID 'HGNC:11998'") {

    val coding = Gene.Coding.apply(
      hgncId = Some(Gene.HgncId("HGNC:11998")),
      ensemblId = None,
      symbol = None,
      name = None
    )


    it("should complete gene coding with Ensembl-ID 'ENSG00000141510'") {

      assert(hgncOps.complete(coding).exists(
        _.ensemblId.contains(Gene.EnsemblId("ENSG00000141510"))
      ))

    }


    it("should complete gene coding with Symbol 'TP53'") {

      assert(hgncOps.complete(coding).exists(
        _.symbol.contains(Gene.Symbol("TP53"))
      ))

    }


    it("should complete gene coding with name 'tumor protein p53'") {

      assert(hgncOps.complete(coding).exists(
        _.name.contains("tumor protein p53"))
      )

    }

  }

  describe("Given gene Ensembl-ID 'ENSG00000141510'") {

    val coding = Gene.Coding.apply(
      hgncId = None,
      ensemblId = Some(Gene.EnsemblId("ENSG00000141510")),
      symbol = None,
      name = None
    )

    it("should complete gene coding with HGNC-ID 'HGNC:11998'") {

      assert(hgncOps.complete(coding).exists(
        _.hgncId.contains(Gene.HgncId("HGNC:11998"))
      ))

    }


    it("should complete gene coding with Symbol 'TP53'") {

      assert(hgncOps.complete(coding).exists(
        _.symbol.contains(Gene.Symbol("TP53"))
      ))

    }


    it("should complete gene coding with name 'tumor protein p53'") {

      assert(hgncOps.complete(coding).exists(
        _.name.contains("tumor protein p53"))
      )

    }

  }

}
