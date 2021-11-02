package de.bwhc.mtb.data.gens


import java.time.Year

import de.bwhc.mtb.data.entry.dtos._


object ICD
{

  lazy val icd10gmCodings =
    Seq(
      "C02.0" -> "Bösartige Neubildung: Zungenrücken",
      "C15.1" -> "Bösartige Neubildung: Thorakaler Ösophagus",
      "C16.0" -> "Bösartige Neubildung: Kardia",
      "C16.3" -> "Bösartige Neubildung: Antrum pyloricum",
      "C17.2" -> "Bösartige Neubildung: Ileum",
      "C18.2" -> "Bösartige Neubildung: Colon ascendens",
      "C22.0" -> "Leberzellkarzinom",
      "C22.1" -> "Intrahepatisches Gallengangskarzinom",
      "C22.2" -> "Hepatoblastom",
      "C22.3" -> "Angiosarkom der Leber",
      "C22.4" -> "Sonstige Sarkome der Leber",
      "C22.9" -> "Bösartige Neubildung: Leber, nicht näher bezeichnet",
      "C24.0" -> "Bösartige Neubildung: Extrahepatischer Gallengang",
      "C24.1" -> "Bösartige Neubildung: Ampulla hepatopancreatica [Ampulla Vateri]",
      "C24.8" -> "Bösartige Neubildung: Gallenwege, mehrere Teilbereiche überlappend",
      "C24.9" -> "Bösartige Neubildung: Gallenwege, nicht näher bezeichnet",
      "C25.0" -> "Bösartige Neubildung: Pankreaskopf",
      "C25.1" -> "Bösartige Neubildung: Pankreaskörper",
      "C25.2" -> "Bösartige Neubildung: Pankreasschwanz",
      "C25.3" -> "Bösartige Neubildung: Ductus pancreaticus",
      "C25.4" -> "Bösartige Neubildung: Endokriner Drüsenanteil des Pankreas",
      "C25.7" -> "Bösartige Neubildung: Sonstige Teile des Pankreas",
      "C25.8" -> "Bösartige Neubildung: Pankreas, mehrere Teilbereiche überlappend",
      "C25.9" -> "Bösartige Neubildung: Pankreas, nicht näher bezeichnet",
      "C26.0" -> "Bösartige Neubildung: Intestinaltrakt, Teil nicht näher bezeichnet",
      "C26.1" -> "Bösartige Neubildung: Milz",
      "C26.8" -> "Bösartige Neubildung: Verdauungssystem, mehrere Teilbereiche überlappend",
      "C26.9" -> "Bösartige Neubildung: Ungenau bezeichnete Lokalisationen des Verdauungssystems",
      "C30.0" -> "Bösartige Neubildung: Nasenhöhle"
    )
    .map { case (c,d) => Coding[ICD10GM](ICD10GM(c),Some(d),Some(Year.now.toString)) }


  lazy val icdO3TCodings =
    Seq(
      "C17.8" -> "Dünndarm, mehrere Teilbereiche überlappend",
      "C17.9" -> "Dünndarm o.n.A.",
      "C18.0" -> "Zökum",
      "C18.1" -> "Appendix vermiformis",
      "C18.2" -> "Colon ascendens",
      "C18.3" -> "Flexura hepatica",
      "C18.4" -> "Colon transversum",
      "C18.5" -> "Flexura lienalis coli",
      "C18.6" -> "Colon descendens",
      "C18.7" -> "Colon sigmoideum",
      "C18.8" -> "Kolon, mehrere Teilbereiche überlappend",
      "C18.9" -> "Colon",
      "C19.9" -> "Rektosigmoidaler Übergang",
      "C20.9" -> "Rektum o.n.A.",
      "C21.0" -> "Anus o.n.A.",
      "C21.1" -> "Analkanal",
      "C21.2" -> "Kloakenregion",
      "C21.8" -> "Rektum, Anus und Analkanal, mehrere Bereiche überlappend",
      "C22.0" -> "Leber",
      "C22.1" -> "Intrahepatische Gallengänge",
      "C23.9" -> "Gallenblase",
      "C24.0" -> "Extrahepatischer Gallengang",
      "C24.1" -> "Ampulla Vateri",
      "C24.8" -> "Gallenwege, mehrere Bereiche überlappend",
      "C24.9" -> "Gallenwege o.n.A.",
      "C25.0" -> "Pankreaskopf",
      "C25.1" -> "Pankreaskörper",
      "C25.2" -> "Pankreasschwanz",
      "C25.3" -> "Ductus pancreaticus",
      "C25.4" -> "Pankreas-Inselzellen",
      "C25.7" -> "Sonstige näher bezeichnete Teile des Pankreas",
      "C25.8" -> "Pankreas, mehrere Teilbereiche überlappend",
      "C25.9" -> "Pankreas o.n.A.",
      "C26.0" -> "Intestinaltrakt o.n.A.",
      "C26.8" -> "Verdauungssystem, mehrere Bereiche überlappend",
      "C26.9" -> "Gastrointestinaltrakt o.n.A."
    )
    .map { case (c,d) => Coding[ICDO3T](ICDO3T(c),Some(d),Some("Zweite Revision")) }


  lazy val icdO3MCodings =
    Seq(
      "8851/3" -> "Gut differenziertes Liposarkom",
      "8852/0" -> "Fibromyxolipom",
      "8852/3" -> "Myxoides Liposarkom",
      "8853/3" -> "Rundzelliges Liposarkom",
      "8854/0" -> "Pleomorphes Lipom",
      "8854/3" -> "Pleomorphes Liposarkom",
      "8855/3" -> "Gemischtzelliges Liposarkom",
      "8856/0" -> "Intramuskuläres Lipom",
      "8857/0" -> "Spindelzell-Lipom",
      "8857/3" -> "Fibroblastisches Liposarkom",
      "8858/3" -> "Entdifferenziertes Liposarkom",
      "8860/0" -> "Angiomyolipom",
      "8861/0" -> "Angiolipom o.n.A.",
      "8862/0" -> "Chondroides Lipom",
      "8870/0" -> "Myelolipom",
      "8880/0" -> "Hibernom",
      "8881/0" -> "Lipoblastomatose",
      "8890/0" -> "Leiomyom o.n.A.",
      "8890/1" -> "Leiomyomatose o.n.A.",
      "8890/3" -> "Leiomyosarkom o.n.A.",
      "8891/0" -> "Epitheloides Leiomyom",
      "8891/3" -> "Epitheloides Leiomyosarkom",
      "8892/0" -> "Zellreiches Leiomyom",
      "8893/0" -> "Bizarres Leiomyom",
      "8894/0" -> "Angiomyom",
      "8894/3" -> "Angiomyosarkom",
      "8895/0" -> "Myom",
      "8895/3" -> "Myosarkom",
      "8896/3" -> "Myxoides Leiomyosarkom",
      "8897/1" -> "Tumor der glatten Muskulatur mit fraglichem malignem Potential",
      "8898/1" -> "Metastasierendes Leiomyom",
      "8900/0" -> "Rhabdomyom o.n.A.",
      "8900/3" -> "Rhabdomyosarkom o.n.A.",
      "8901/3" -> "Adultes pleomorphes Rhabdomyosarkom",
      "8902/3" -> "Rhabdomyosarkom vom Mischtyp",
      "8903/0" -> "Fetales Rhabdomyom",
      "8904/0" -> "Adultes Rhabdomyom",
      "8905/0" -> "Genitales RhabdomyomC51.-C52.9",
      "8910/3" -> "Embryonales Rhabdomyosarkom",
      "8912/3" -> "Spindelzelliges Rhabdomyosarkom",
      "8920/3" -> "Alveoläres Rhabdomyosarkom",
      "8921/3" -> "Rhabdomyosarkom mit ganglionärer Differenzierung"
    )
    .map { case (c,d) => Coding[ICDO3M](ICDO3M(c),Some(d),Some("Zweite Revision")) }

}
