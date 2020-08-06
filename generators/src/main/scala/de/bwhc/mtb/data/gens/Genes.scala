package de.bwhc.mtb.data.gens



import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Variant
}


object Genes
{

  lazy val entries =
    Seq(
      "ADTRP" -> "androgen dependent TFPI regulating protein",
      "AEBP1" -> "AE binding protein 1",
      "AEBP2" -> "AE binding protein 2",
      "AEN" -> "apoptosis enhancing nuclease",
      "AFAP1" -> "actin filament associated protein 1",
      "AFAP1L1" -> "actin filament associated protein 1 like 1",
      "AFAP1L2" -> "actin filament associated protein 1 like 2",
      "AFDN" -> "afadin, adherens junction formation factor",
      "AFF1" -> "AF4/FMR2 family member 1",
      "AFF2" -> "AF4/FMR2 family member 2",
      "AFF3" -> "AF4/FMR2 family member 3",
      "BPIFB4" -> "BPI fold containing family B member 4",
      "BPIFB6" -> "BPI fold containing family B member 6",
      "BPIFC" -> "BPI fold containing family C",
      "BPNT1" -> "3'(2'), 5'-bisphosphate nucleotidase 1",
      "BPNT2" -> "3'(2'), 5'-bisphosphate nucleotidase 2",
      "BPTF" -> "bromodomain PHD finger transcription factor",
      "BPY2" -> "basic charge Y-linked 2",
      "BPY2B" -> "basic charge Y-linked 2B",
      "BPY2C" -> "basic charge Y-linked 2C",
      "BRAF" -> "B-Raf proto-oncogene, serine/threonine kinase",
      "BRAP" -> "BRCA1 associated protein",
      "BRAT1" -> "BRCA1 associated ATM activator 1",
      "BRCA1" -> "BRCA1 DNA repair associated",
      "BRCA2" -> "BRCA2 DNA repair associated",
      "BRCC3" -> "BRCA1/BRCA2-containing complex subunit 3",
      "BRD1" -> "bromodomain containing 1",
      "BRD2" -> "bromodomain containing 2",
      "BRD3" -> "bromodomain containing 3",
      "BRD3OS" -> "BRD3 opposite strand",
      "BTNL3" -> "butyrophilin like 3",
      "BTNL8" -> "butyrophilin like 8",
      "BTNL9" -> "butyrophilin like 9",
      "BTRC" -> "beta-transducin repeat containing E3 ubiquitin protein ligase",
      "BUB1" -> "BUB1 mitotic checkpoint serine/threonine kinase",
      "BUB1B" -> "BUB1 mitotic checkpoint serine/threonine kinase B",
      "BUB3" -> "BUB3 mitotic checkpoint protein",
      "BUD13" -> "BUD13 homolog",
      "BUD23" -> "BUD23 rRNA methyltransferase and ribosome maturation factor",
      "BUD31" -> "BUD31 homolog",
      "BVES" -> "blood vessel epicardial substance",
      "BYSL" -> "bystin like",
      "BZW1" -> "basic leucine zipper and W2 domains 1",
      "BZW2" -> "basic leucine zipper and W2 domains 2",
      "C1D" -> "C1D nuclear receptor corepressor",
      "C1GALT1" -> "core 1 synthase, glycoprotein-N-acetylgalactosamine 3-beta-galactosyltransferase 1",
      "C1GALT1C1" -> "C1GALT1 specific chaperone 1",
      "C1GALT1C1L" -> "C1GALT1 specific chaperone 1 like",
      "C1orf21" -> "chromosome 1 open reading frame 21",
      "C1orf35" -> "chromosome 1 open reading frame 35",
      "C1orf43" -> "chromosome 1 open reading frame 43",
      "TOP3B" -> "DNA topoisomerase III beta",
      "TOPAZ1" -> "testis and ovary specific PAZ domain containing 1",
      "TOPBP1" -> "DNA topoisomerase II binding protein 1",
      "TOPORS" -> "TOP1 binding arginine/serine rich protein",
      "TOR1A" -> "torsin family 1 member A",
      "TOR1AIP1" -> "torsin 1A interacting protein 1",
      "TOR1AIP2" -> "torsin 1A interacting protein 2",
      "TOR1B" -> "torsin family 1 member B",
      "TOR2A" -> "torsin family 2 member A",
      "TOR3A" -> "torsin family 3 member A",
      "TOR4A" -> "torsin family 4 member A",
      "TOX" -> "thymocyte selection associated high mobility group box",
      "TOX2" -> "TOX high mobility group box family member 2",
      "TOX3" -> "TOX high mobility group box family member 3",
      "TOX4" -> "TOX high mobility group box family member 4",
      "TP53" -> "tumor protein p53",
      "TP53AIP1" -> "tumor protein p53 regulated apoptosis inducing protein 1",
      "TP53BP1" -> "tumor protein p53 binding protein 1",
      "TP53BP2" -> "tumor protein p53 binding protein 2",
      "TP53I3" -> "tumor protein p53 inducible protein 3",
      "TP53I11" -> "tumor protein p53 inducible protein 11",
      "TP53I13" -> "tumor protein p53 inducible protein 13",
      "TP53INP1" -> "tumor protein p53 inducible nuclear protein 1",
      "TP53INP2" -> "tumor protein p53 inducible nuclear protein 2",
      "TP53RK" -> "TP53 regulating kinase",
      "TP53TG3" -> "TP53 target 3",
      "TP53TG3B" -> "TP53 target 3B",
      "TP53TG3C" -> "TP53 target 3C",
      "TP53TG3D" -> "TP53 target 3D",
      "TP53TG3E" -> "TP53 target 3 family member E",
      "TP53TG3F" -> "TP53 target 3 family member F",
      "TP53TG5" -> "TP53 target 5",
      "TP63" -> "tumor protein p63",
      "TP73" -> "tumor protein p73",
      "ZUP1" -> "zinc finger containing ubiquitin peptidase 1",
      "ZW10" -> "zw10 kinetochore protein",
      "ZWILCH" -> "zwilch kinetochore protein",
      "ZWINT" -> "ZW10 interacting kinetochore protein",
      "ZXDA" -> "zinc finger X-linked duplicated A",
      "ZXDB" -> "zinc finger X-linked duplicated B",
      "ZXDC" -> "ZXD family zinc finger C",
      "ZYG11A" -> "zyg-11 family member A, cell cycle regulator",
      "ZYG11B" -> "zyg-11 family member B, cell cycle regulator",
      "ZYX" -> "zyxin",
      "ZZEF1" -> "zinc finger ZZ-type and EF-hand domain containing 1",
      "ZZZ3" -> "zinc finger ZZ-type containing 3",
    )
    .map { case (c,d) => Coding[Variant.Gene](Variant.Gene(c),Some(d)) }
 

}
