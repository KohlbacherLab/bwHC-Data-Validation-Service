package de.bwhc.mtb.data.entry.dtos



import de.bwhc.catalogs.med.{
  MedicationCatalog => ATCCatalog,
  Medication => ATCMedication
}
import de.bwhc.catalogs.icd.{
  ICD10GMCoding,
  ICD10GMCatalogs,
  ICD10GM => ICD10,
  ICDO3Catalogs,
}
import de.bwhc.catalogs.hgnc.{
  HGNCCatalog,
  HGNCGene,
  HGNCId
}


object CodingExtensions
{


  implicit class ICD10CodingOps(val coding: Coding[ICD10GM]) extends AnyVal
  {

    def complete(
      implicit catalogs: ICD10GMCatalogs
    ): Coding[ICD10GM] = {
      (
        coding.version match {
          case None =>
            catalogs.coding(ICD10.Code(coding.code.value))
        
          case Some(version) =>
            catalogs.coding(ICD10.Code(coding.code.value),version)
        }
      )
      .map(icd10 => coding.copy(display = Some(icd10.display)))
      .getOrElse(coding)
    }

  }


  implicit class ICDO3TCodingOps(val coding: Coding[ICDO3T]) extends AnyVal
  {

    def complete(
      implicit catalogs: ICDO3Catalogs
    ): Coding[ICDO3T] = {
      (
        coding.version match {
          case None =>
            catalogs.topographyCodings()
              .find(_.code.value == coding.code.value)
        
          case Some(version) =>
            catalogs.topographyCodings(version)
              .find(_.code.value == coding.code.value)
        }
      )
      .map(icdo3 => coding.copy(display = Some(icdo3.display)))
      .getOrElse(coding)
    }

  }


  implicit class ICDO3MCodingOps(val coding: Coding[ICDO3M]) extends AnyVal
  {

    def complete(
      implicit catalogs: ICDO3Catalogs
    ): Coding[ICDO3M] = {
      (
        coding.version match {
          case None =>
            catalogs.morphologyCodings()
              .find(_.code.value == coding.code.value)
        
          case Some(version) =>
            catalogs.morphologyCodings(version)
              .find(_.code.value == coding.code.value)
        }
      )
      .map(icdo3 => coding.copy(display = Some(icdo3.display)))
      .getOrElse(coding)
    }

  }


  implicit class MedicationCodingOps(val coding: Medication.Coding) extends AnyVal
  {

    import Medication.System._

    def complete(
      implicit atcCatalogs: ATCCatalog
    ): Medication.Coding = 
      coding.system match {
        case ATC =>
          coding.version
            .flatMap(atcCatalogs.findWithCode(coding.code.value,_))
            .map(atc => coding.copy(display = Some(atc.name)))
            .getOrElse(coding)

        case Unregistered => coding

      }  

  }


  implicit class GeneCodingOps(val coding: Gene.Coding) extends AnyVal
  {

    def complete(
      implicit hgnc: HGNCCatalog[cats.Id]
    ): Gene.Coding = {

      import Gene._
      
      coding.hgncId.flatMap(
        id => hgnc.gene(HGNCId(id.value))
      )
      .orElse(
        coding.ensemblId.flatMap(
          id => hgnc.geneWithEnsemblId(id.value)
        )
      )
      .map(
        gene =>
          coding.copy(
            ensemblId = gene.ensemblId.map(id => EnsemblId(id.value)),
            hgncId = Some(HgncId(gene.hgncId.value)),
            symbol = Some(Symbol(gene.symbol)),
            name = Some(gene.name)
          )
      )
      .getOrElse(coding)

    }
  }


}
