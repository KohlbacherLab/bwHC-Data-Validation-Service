package de.bwhc.mtb.data.gens


import de.bwhc.mtb.data.entry.dtos.{
  Coding,Medication
}


object Medications
{

  lazy val entries =
    Seq(
      "L01XE13" -> "Afatinib",
      "L01XE36" -> "Alectinib",
      "L01DB10" -> "Amrubicin",
      "L01XC32" -> "Atezolizumab",
      "L01XC31" -> "Avelumab",
      "L01XC07" -> "Bevacizumab",
      "L01XE41" -> "Binimetinib",
      "L01XX32" -> "Bortezomib",
      "L01XE14" -> "Bosutinib",
      "L01XE43" -> "Brigatinib",
      "L01CD04" -> "Cabazitaxel",
      "L01XE26" -> "Cabozantinib",
      "L01BC06" -> "Capecitabin",
      "L01XA02" -> "Carboplatin",
      "L01XE28" -> "Ceritinib",
      "L01XC06" -> "Cetuximab",
      "L01XA01" -> "Cisplatin",
      "L01XE38" -> "Cobimetinib",
      "L01XE16" -> "Crizotinib",
      "L01XE23" -> "Dabrafenib",
      "L01XE47" -> "Dacomitinib",
      "L01XE06" -> "Dasatinib",
      "L01DB02" -> "Daunorubicin",
      "L01CD02" -> "Docetaxel",
      "L01DB01" -> "Doxorubicin",
      "L01XC28" -> "Durvalumab",
      "L01XX59" -> "Enasidenib",
      "L01XE46" -> "Encorafenib",
      "L01DB03" -> "Epirubicin",
      "L01XE03" -> "Erlotinib",
      "L01XE10" -> "Everolimus",
      "C04AX32" -> "Fasudil",
      "L02BA03" -> "Fulvestrant",
      "L01XE02" -> "Gefitinib",
      "L01XE27" -> "Ibrutinib",
      "L01DB06" -> "Idarubicin",
      "L01XX47" -> "Idelalisib",
      "L01XE01" -> "Imatinib",
      "L01XX19" -> "Irinotecan",
      "L01XX50" -> "Ixazomib",
      "L01XE07" -> "Lapatinib",
      "L01XE44" -> "Lorlatinib",
      "M01AC06" -> "Meloxicam",
      "A10BA02" -> "Metformin",
      "L01XE39" -> "Midostaurin",
      "L01XE45" -> "Neratinib",
      "L01XE08" -> "Nilotinib",
      "L01XC17" -> "Nivolumab",
      "L01XX46" -> "Olaparib",
      "L01XE35" -> "Osimertinib",
      "L01XA03" -> "Oxaliplatin",
      "L01CD01" -> "Paclitaxel",
      "L01XE33" -> "Palbociclib",
      "L01XC08" -> "Panitumumab",
      "L01XX42" -> "Panobinostat",
      "L01XE11" -> "Pazopanib",
      "L01XC18" -> "Pembrolizumab",
      "L01BA04" -> "Pemetrexed",
      "L01XC13" -> "Pertuzumab",
      "L01XE24" -> "Ponatinib",
      "L01BA03" -> "Raltitrexed",
      "L01XE21" -> "Regorafenib",
      "L01XE19" -> "Ridaforolimus",
      "L01XE37" -> "Rociletinib",
      "L01XE18" -> "Ruxolitinib",
      "L01XX48" -> "Sonidegib",
      "L01XE05" -> "Sorafenib",
      "L01XE04" -> "Sunitinib",
      "L02BA01" -> "Tamoxifen",
      "L01XE09" -> "Temsirolimus",
      "L01XX17" -> "Topotecan",
      "L01XE25" -> "Trametinib",
      "L01XC03" -> "Trastuzumab",
      "L01XE12" -> "Vandetanib",
      "L01XE15" -> "Vemurafenib",
      "L01XX43" -> "Vismodegib",
      "L01XX38" -> "Vorinostat"
   )
   .map { case (c,d) => Medication.Coding(Medication.Code(c),Medication.System.ATC,Some(d),Some("2020")) } 
//   .map { case (c,d) => Medication.Coding(Medication.Code(c),Medication.System.ATC,Some(d),None) } 
  
  
  
} 
