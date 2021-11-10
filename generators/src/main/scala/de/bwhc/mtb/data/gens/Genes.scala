package de.bwhc.mtb.data.gens



import de.bwhc.mtb.data.entry.dtos.{
  Coding,
  Variant
}


object Genes
{

  import Variant.{HgncId,GeneSymbol}

  val entries: Seq[(Coding[Variant.HgncId],Coding[Variant.GeneSymbol])] =
    Seq(
      ("HGNC:37133","A1BG-AS1","A1BG antisense RNA 1"),
      ("HGNC:23336","A2ML1","alpha-2-macroglobulin like 1"),
      ("HGNC:8","A2MP1","alpha-2-macroglobulin pseudogene 1"),
      ("HGNC:30005","A3GALT2","alpha 1,3-galactosyltransferase 2"),
      ("HGNC:18149","A4GALT","alpha 1,4-galactosyltransferase (P blood group),"),
      ("HGNC:18226","AACSP1","acetoacetyl-CoA synthetase pseudogene 1"),
      ("HGNC:30205","AAMDC","adipogenesis associated Mth938 domain containing"),
      ("HGNC:15886","AAR2","AAR2 splicing factor"),
      ("HGNC:33842","AARD","alanine and arginine rich domain containing protein"),
      ("HGNC:20","AARS1","alanyl-tRNA synthetase 1"),
      ("HGNC:49894","AARS1P1","alanyl-tRNA synthetase 1 pseudogene 1"),
      ("HGNC:21022","AARS2","alanyl-tRNA synthetase 2, mitochondrial"),
      ("HGNC:29","ABCA1","ATP binding cassette subfamily A member 1"),
      ("HGNC:32","ABCA2","ATP binding cassette subfamily A member 2"),
      ("HGNC:33","ABCA3","ATP binding cassette subfamily A member 3"),
      ("HGNC:34","ABCA4","ATP binding cassette subfamily A member 4"),
      ("HGNC:31","ABCA11P","ATP binding cassette subfamily A member 11, pseudogene"),
      ("HGNC:14637","ABCA12","ATP binding cassette subfamily A member 12"),
      ("HGNC:40","ABCB1","ATP binding cassette subfamily B member 1"),
      ("HGNC:45","ABCB4","ATP binding cassette subfamily B member 4"),
      ("HGNC:48","ABCB7","ATP binding cassette subfamily B member 7"),
      ("HGNC:14114","ABCB10P1","ABCB10 pseudogene 1"),
      ("HGNC:42","ABCB11","ATP binding cassette subfamily B member 11"),
      ("HGNC:51","ABCC1","ATP binding cassette subfamily C member 1"),
      ("HGNC:53","ABCC2","ATP binding cassette subfamily C member 2"),
      ("HGNC:57","ABCC6","ATP binding cassette subfamily C member 6"),
      ("HGNC:59","ABCC8","ATP binding cassette subfamily C member 8"),
      ("HGNC:61","ABCD1","ATP binding cassette subfamily D member 1"),
      ("HGNC:66","ABCD2","ATP binding cassette subfamily D member 2"),
      ("HGNC:67","ABCD3","ATP binding cassette subfamily D member 3"),
      ("HGNC:68","ABCD4","ATP binding cassette subfamily D member 4"),
      ("HGNC:69","ABCE1","ATP binding cassette subfamily E member 1"),
      ("HGNC:70","ABCF1","ATP binding cassette subfamily F member 1"),
      ("HGNC:54751","ABCF2-H2BK1","ABCF2-H2BK1 readthrough"),
      ("HGNC:16407","ABHD11","abhydrolase domain containing 11"),
      ("HGNC:18289","ABHD11-AS1","ABHD11 antisense RNA 1 (tail to tail),"),
      ("HGNC:15868","ABHD12","abhydrolase domain containing 12, lysophospholipase"),
      ("HGNC:19837","ABHD12B","abhydrolase domain containing 12B"),
      ("HGNC:20293","ABHD13","abhydrolase domain containing 13"),
      ("HGNC:13921","ABHD16A","abhydrolase domain containing 16A, phospholipase"),
      ("HGNC:16128","ABHD16B","abhydrolase domain containing 16B"),
      ("HGNC:28756","ABHD17A","abhydrolase domain containing 17A, depalmitoylase"),
      ("HGNC:28394","ABHD17AP1","ABHD17A pseudogene 1"),
      ("HGNC:34041","ABHD17AP3","ABHD17A pseudogene 3"),
      ("HGNC:34042","ABHD17AP4","ABHD17A pseudogene 4"),
      ("HGNC:34043","ABHD17AP5","ABHD17A pseudogene 5"),
      ("HGNC:34044","ABHD17AP6","ABHD17A pseudogene 6"),
      ("HGNC:38508","ABHD17AP7","ABHD17A pseudogene 7"),
      ("HGNC:38509","ABHD17AP8","ABHD17A pseudogene 8"),
      ("HGNC:38510","ABHD17AP9","ABHD17A pseudogene 9"),
      ("HGNC:24278","ABHD17B","abhydrolase domain containing 17B, depalmitoylase"),
      ("HGNC:26925","ABHD17C","abhydrolase domain containing 17C, depalmitoylase"),
      ("HGNC:26111","ABHD18","abhydrolase domain containing 18"),
      ("HGNC:11320","ABI1","abl interactor 1"),
      ("HGNC:20035","ABI1P1","abl interactor 1 pseudogene 1"),
      ("HGNC:1364","ABITRAM","actin binding transcription modulator"),
      ("HGNC:39851","ABITRAMP1","ABITRAM pseudogene 1"),
      ("HGNC:76","ABL1","ABL proto-oncogene 1, non-receptor tyrosine kinase"),
      ("HGNC:77","ABL2","ABL proto-oncogene 2, non-receptor tyrosine kinase"),
      ("HGNC:78","ABLIM1","actin binding LIM protein 1"),
      ("HGNC:21230","ABRACL","ABRA C-terminal like"),
      ("HGNC:25829","ABRAXAS1","abraxas 1, BRCA1 A complex subunit"),
      ("HGNC:28975","ABRAXAS2","abraxas 2, BRISC complex subunit"),
      ("HGNC:84","ACACA","acetyl-CoA carboxylase alpha"),
      ("HGNC:319","ACAN","aggrecan"),
      ("HGNC:16467","ACAP1","ArfGAP with coiled-coil, ankyrin repeat and PH domains 1"),
      ("HGNC:16469","ACAP2","ArfGAP with coiled-coil, ankyrin repeat and PH domains 2"),
      ("HGNC:16754","ACAP3","ArfGAP with coiled-coil, ankyrin repeat and PH domains 3"),
      ("HGNC:93","ACAT1","acetyl-CoA acetyltransferase 1"),
      ("HGNC:15453","ACBD3","acyl-CoA binding domain containing 3"),
      ("HGNC:2707","ACE","angiotensin I converting enzyme"),
      ("HGNC:18356","ACER1","alkaline ceramidase 1"),
      ("HGNC:23675","ACER2","alkaline ceramidase 2"),
      ("HGNC:16066","ACER3","alkaline ceramidase 3"),
      ("HGNC:108","ACHE","acetylcholinesterase (Cartwright blood group),"),
      ("HGNC:17066","ACIN1","apoptotic chromatin condensation inducer 1"),
      ("HGNC:4035","ACKR1","atypical chemokine receptor 1 (Duffy blood group),"),
      ("HGNC:1565","ACKR2","atypical chemokine receptor 2"),
      ("HGNC:23692","ACKR3","atypical chemokine receptor 3"),
      ("HGNC:1611","ACKR4","atypical chemokine receptor 4"),
      ("HGNC:21142","ACKR4P1","ACKR4 pseudogene 1"),
      ("HGNC:34504","ACNATP","acyl-CoA:amino acid N-acyltransferase pseudogene"),
      ("HGNC:117","ACO1","aconitase 1"),
      ("HGNC:33904","ACOD1","aconitate decarboxylase 1"),
      ("HGNC:33159","ACOT6","acyl-CoA thioesterase 6"),
      ("HGNC:15919","ACOT8","acyl-CoA thioesterase 8"),
      ("HGNC:18156","ACOT11","acyl-CoA thioesterase 11"),
      ("HGNC:20999","ACOT13","acyl-CoA thioesterase 13"),
      ("HGNC:125","ACP3","acid phosphatase 3"),
      ("HGNC:14376","ACP4","acid phosphatase 4"),
      ("HGNC:3569","ACSL1","acyl-CoA synthetase long chain family member 1"),
      ("HGNC:3570","ACSL3","acyl-CoA synthetase long chain family member 3"),
      ("HGNC:3571","ACSL4","acyl-CoA synthetase long chain family member 4"),
      ("HGNC:16526","ACSL5","acyl-CoA synthetase long chain family member 5"),
      ("HGNC:16496","ACSL6","acyl-CoA synthetase long chain family member 6"),
      ("HGNC:18049","ACSM1","acyl-CoA synthetase medium chain family member 1"),
      ("HGNC:32017","ACSM2A","acyl-CoA synthetase medium chain family member 2A"),
      ("HGNC:30931","ACSM2B","acyl-CoA synthetase medium chain family member 2B"),
      ("HGNC:10522","ACSM3","acyl-CoA synthetase medium chain family member 3"),
      ("HGNC:31665","ACSM6","acyl-CoA synthetase medium chain family member 6")
    )
    .map {
      case (id,sym,name) => (Coding(HgncId(id),None) -> Coding(GeneSymbol(sym),Some(name)))
    }


}
