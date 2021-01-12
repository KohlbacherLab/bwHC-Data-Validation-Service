package de.bwhc.mtb.data.entry.impl


import java.time.LocalDate
import java.time.temporal.Temporal

import scala.util.Either
import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.NonEmptyList
import cats.data.Validated

import de.bwhc.util.spi._
import de.bwhc.util.data.ClosedInterval
import de.bwhc.util.data.Validation._
import de.bwhc.util.data.Validation.dsl._

import de.bwhc.mtb.data.entry.dtos
import de.bwhc.mtb.data.entry.dtos._
import de.bwhc.mtb.data.entry.api.DataQualityReport

import de.bwhc.catalogs.icd
import de.bwhc.catalogs.icd._
import de.bwhc.catalogs.hgnc.{HGNCGene,HGNCCatalog}
import de.bwhc.catalogs.med.MedicationCatalog



trait DataValidator
{

  def check(
    mtbfile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[Validated[DataQualityReport,MTBFile]]

}

trait DataValidatorProvider extends SPI[DataValidator]

object DataValidator extends SPILoader(classOf[DataValidatorProvider])



class DefaultDataValidator extends DataValidator
{

  import DefaultDataValidator._

  def check(
    mtbfile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[Validated[DataQualityReport,MTBFile]] = {
    Future.successful(
      mtbfile.validate
        .leftMap(DataQualityReport(mtbfile.patient.id,_))
    )
  }

}


object DefaultDataValidator
{

  import DataQualityReport._
  import DataQualityReport.Issue._

  import cats.syntax.apply._
  import cats.syntax.traverse._
  import cats.syntax.validated._
  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.set._



  type DataQualityValidator[T] = Validator[DataQualityReport.Issue,T]


  private def validReference[Ref](
    location: => Location,
  )(
    implicit ref: Ref
  ): DataQualityValidator[Ref] = {
    r =>
      r must equal (ref) otherwise (
        Fatal(s"Invalid Reference to $r") at location
      )
  }

  private def validReference[Ref, C[X] <: Iterable[X]](
    refs: C[Ref]
  )(
    location: => Location,
  ): DataQualityValidator[Ref] = {
    r =>
      r must be (in (refs)) otherwise (
        Fatal(s"Invalid Reference to $r") at location
      )
  }

  private def validReferences[Ref](
    location: => Location,
  )(
    implicit refs: Iterable[Ref]
  ): DataQualityValidator[List[Ref]] = {
    rs =>
      rs validateEach (
        r => r must be (in (refs)) otherwise (
          Fatal(s"Invalid Reference to $r") at location
        )
      )
  }



  implicit val patientValidator: DataQualityValidator[Patient] = {

    case pat @ Patient(Patient.Id(id),_,birthDate,_,insurance,dod) =>

      (
        birthDate mustBe defined otherwise (Error("Missing BirthDate") at Location("Patient",id,"birthdate")),

        insurance shouldBe defined otherwise (Warning("Missing Health Insurance") at Location("Patient",id,"insurance")),

        (dod couldBe defined otherwise (
           Info("Undefined date of death. Ensure if up to date") at Location("Patient",id,"dateOfDeath"))
          ) andThen ( date =>
            date.get must be (before (LocalDate.now))
              otherwise (Error("Invalid Date of death in the future") at Location("Patient",id,"dateOfDeath"))
          ),

        (birthDate, dod)
          .mapN(
            (b,d) => d must be (after (b))
                       otherwise (Error("Invalid Date of death before birthDate") at Location("Patient",id,"dateOfDeath"))
           )
          .getOrElse(LocalDate.now.validNel[Issue])
      )
      .mapN { case _: Product => pat }
  }


  implicit def consentValidator(
    implicit patId: Patient.Id
  ): DataQualityValidator[Consent] = {

    case consent @ Consent(id,patient,_) =>

      (patient must be (validReference[Patient.Id](Location("Consent",id.value,"patient"))))
        .map(_ => consent)

  }


  implicit def episodeValidator(
    implicit patId: Patient.Id
  ): DataQualityValidator[MTBEpisode] = {
    case episode @ MTBEpisode(id,patient,period) =>

      (patient must be (validReference[Patient.Id](Location("MTBEpisode",id.value,"patient"))))
        .map(_ => episode)

  }


  implicit lazy val icd10gmCatalog = ICD10GMCatalogs.getInstance.get

  implicit lazy val icdO3Catalog   = ICDO3Catalogs.getInstance.get


  implicit def icd10Validator(
    implicit
    catalog: ICD10GMCatalogs
  ): DataQualityValidator[Coding[ICD10GM]] = {

      case icd10 @ Coding(dtos.ICD10GM(code),_,version) =>

        (version mustBe defined otherwise (Error("Missing ICD-10-GM Version") at Location("ICD-10-GM Coding","","version")))
          .andThen ( v =>
            attempt(icd.ICD10GM.Version(v.get)) otherwise (
              Error(s"Invalid ICD-10-GM Version '${v.get}'") at Location("ICD-10-GM Coding","","version")
            )
          )
          .andThen (
            v =>
              code must be (in (catalog.codings(v).map(_.code.value)))
                otherwise (Error(s"Invalid ICD-10-GM code '$code'") at Location("ICD-10-GM Coding","","code"))
          )
          .map(c => icd10)

    }


  implicit def icdO3TValidator(
    implicit
    catalog: ICDO3Catalogs
  ): DataQualityValidator[Coding[ICDO3T]] = {

      case icdo3t @ Coding(ICDO3T(code),_,version) =>

        (version mustBe defined otherwise (Error("Missing ICD-O-3 Version") at Location("ICD-O-3-T Coding","","version")))
          .andThen(
            v => 
              attempt(icd.ICDO3.Version(v.get)) otherwise (
                Error(s"Invalid ICD-O-3 Version '${v.get}'") at Location("ICD-O-3-T Coding","","version")
              )
          )
          .andThen(
            v =>
              code must be (in (catalog.topographyCodings(v).map(_.code.value)))
                otherwise (Error(s"Invalid ICD-O-3-T code '$code'") at Location("ICD-O-3-T Coding","","code"))
          )
          .map(c => icdo3t)

    }


  implicit def icdO3MValidator(
    implicit
    catalog: ICDO3Catalogs
  ): DataQualityValidator[Coding[ICDO3M]] = {

      case icdo3m @ Coding(ICDO3M(code),_,version) =>

        (version mustBe defined otherwise (Error("Missing ICD-O-3 Version") at Location("ICD-O-3-M Coding","","version")))
          .andThen(
            v =>
              attempt(icd.ICDO3.Version(v.get)) otherwise (
                Error(s"Invalid ICD-O-3 Version '${v.get}'") at Location("ICD-O-3-M Coding","","version")
              )
          )
          .andThen(
            v =>
              code must be (in (catalog.morphologyCodings(v).map(_.code.value)))
                otherwise (Error(s"Invalid ICD-O-3-M code '$code'") at Location("ICD-O-3-M Coding","","code"))
          )
          .map(c => icdo3m)

    }


  implicit val medicationCatalog = MedicationCatalog.getInstance.get

  implicit def medicationValidator(
    implicit
    catalog: MedicationCatalog
  ): DataQualityValidator[Coding[Medication]] = {

    case medication @ Coding(Medication(atcCode),_,_) =>

      (atcCode must be (in (catalog.entries.map(_.code.value)))
        otherwise (
          Error(s"Invalid ATC Medication code '$atcCode'") at Location("Medication Coding","","code"))
       )
       .map(c => medication)

  }



  implicit def diagnosisValidator(
    implicit
    patId: Patient.Id,
    specimenRefs: List[Specimen.Id],
    histologyRefs: List[HistologyReport.Id]
  ): DataQualityValidator[Diagnosis] = {

    case diag @ Diagnosis(Diagnosis.Id(id),patient,date,icd10,icdO3T,_,histologyReportRefs,_,glTreatmentStatus) =>

      implicit val diagId = diag.id

      (
        patient must be (validReference[Patient.Id](Location("Diagnosis",id,"patient"))),

        date shouldBe defined otherwise (Warning("Missing Recording Date") at Location("Diagnosis",id,"recordedOn")),

        (icd10 mustBe defined otherwise (Error("Missing ICD-10-GM Coding") at Location("Diagnosis",id,"icd10")))
          andThen (_.get validate),

        (icdO3T couldBe defined otherwise (Info("Missing ICD-O-3-T Coding") at Location("Diagnosis",id,"icdO3T")))
          andThen (_.get validate),

        histologyReportRefs
          .map(_ must be (validReferences[HistologyReport.Id](Location("Diagnosis",id,"histologyReports"))))
          .getOrElse(List.empty[HistologyReport.Id].validNel[Issue]), 

        glTreatmentStatus mustBe defined otherwise (
          Warning("Missing Guideline Therapy Treatment Status") at Location("Diagnosis",id,"guidelineTreatmentStatus")
        )

      )
      .mapN { case _: Product => diag }

  }


  implicit def famMemberDiagnosisValidator(
    implicit
    patId: Patient.Id,
  ): DataQualityValidator[FamilyMemberDiagnosis] = {

    diag =>
      (diag.patient must be (validReference[Patient.Id](Location("FamilyMemberDiagnosis",diag.id.value,"patient"))))
       .map(ref => diag)
  }


  implicit val therapyLines = (0 to 9).map(TherapyLine(_))
  
  implicit def prevGuidelineTherapyValidator(
    implicit
    patId: Patient.Id,
    diagnosisRefs: List[Diagnosis.Id],
    therapyLines: Seq[TherapyLine]
  ): DataQualityValidator[PreviousGuidelineTherapy] = {

    case th @ PreviousGuidelineTherapy(TherapyId(id),patient,diag,therapyLine,medication) =>
      (
        (patient must be (validReference[Patient.Id](Location("PreviousGuidelineTherapy",id,"patient")))),

        diag must be (validReference(diagnosisRefs)(Location("PreviousGuidelineTherapy",id,"diagnosis"))),

        (therapyLine ifUndefined (Warning("Missing Therapy Line") at Location("PreviousGuidelineTherapy",id,"therapyLine")))
          andThen ( l =>
            l must be (in (therapyLines)) otherwise (Error(s"Invalid Therapy Line '${l.value}'") at Location("PreviousGuidelineTherapy",id,"therapyLine"))
          ),

        medication.toList.validateEach
        
      )
      .mapN { case _: Product => th }
  }


  implicit def lastGuidelineTherapyValidator(
    implicit
    patId: Patient.Id,
    diagnosisRefs: List[Diagnosis.Id],
    therapyLines: Seq[TherapyLine],
    therapyRefs: Seq[TherapyId],
  ): DataQualityValidator[LastGuidelineTherapy] = {

    case th @ LastGuidelineTherapy(TherapyId(id),patient,diag,therapyLine,period,medication,reasonStopped) =>
      (
        (patient must be (validReference[Patient.Id](Location("LastGuidelineTherapy",id,"patient")))),

        diag must be (validReference(diagnosisRefs)(Location("LastGuidelineTherapy",id,"diagnosis"))),

        period ifUndefined (Warning("Missing Therapy Period (Start/End)") at Location("LastGuidelineTherapy",id,"period"))
          andThen (
            p => p.end shouldBe defined otherwise (Warning("Missing Therapy end date") at Location("LastGuidelineTherapy",id,"period"))
          ),

        (therapyLine ifUndefined (Warning("Missing Therapy Line") at Location("LastGuidelineTherapy",id,"therapyLine")))
          andThen ( l =>
            l must be (in (therapyLines)) otherwise (Error(s"Invalid Therapy Line '${l.value}'") at Location("LastGuidelineTherapy",id,"therapyLine"))
          ),
        
        medication.toList.validateEach,

        (reasonStopped ifUndefined (Warning("Missing Stop Reason") at Location("LastGuidelineTherapy",id,"reasonStopped"))),

        (th.id must be (in (therapyRefs))
           otherwise (Warning("Missing Response") at Location("LastGuidelineTherapy",id,"response")))
      )
      .mapN { case _: Product => th }

  }


  implicit def ecogStatusValidator(
    implicit
    patId: Patient.Id
  ): DataQualityValidator[ECOGStatus] = {

    case pfSt @ ECOGStatus(id,patient,date,value) =>

      (
        (patient must be (validReference[Patient.Id](Location("ECOGStatus",id.value,"patient")))),

        date mustBe defined otherwise (
          Error("Missing ECOG Performance Status Recording Date") at Location("ECOGStatus",id.value,"effectiveDate")
        ),
      )
      .mapN { case _: Product => pfSt }

  }


  implicit def specimenValidator(
    implicit
    patId: Patient.Id,
    icd10codes: Seq[ICD10GM]
  ): DataQualityValidator[Specimen] = {

    case sp @ Specimen(Specimen.Id(id),patient,icd10,typ,collection) =>
      (
        (patient must be (validReference[Patient.Id](Location("Specimen",id,"patient")))),

        icd10.validate
          andThen (
            icd =>
              icd.code must be (in (icd10codes)) otherwise (
                Fatal(s"Invalid Reference to Diagnosis $icd") at Location("Specimen",id,"icd10")
              )
          ),
  
        (typ ifUndefined (Warning(s"Missing Specimen type") at Location("Specimen",id,"type"))),

        (collection ifUndefined (Warning(s"Missing Specimen collection") at Location("Specimen",id,"collection")))
       
      )
      .mapN { case _: Product => sp }

  }



  import scala.math.Ordering.Double.TotalOrdering

  private val tcRange = ClosedInterval(0.0 -> 1.0)

  implicit def tumorContentValidator(
    implicit specimens: Seq[Specimen.Id]
  ): DataQualityValidator[TumorCellContent] = {

    case tc @ TumorCellContent(TumorCellContent.Id(id),specimen,method,value) => {

      (
        (value must be (in (tcRange))
          otherwise (
            Error(s"Tumor content value '$value' not in reference range $tcRange") at Location("TumorContent",id,"value"))
          )
          .map(_ => tc),

        specimen must be (validReference(specimens)(Location("TumorContent",id,"specimen")))
      )
      .mapN { case _: Product => tc }
  
    }
     
  }


  implicit def tumorMorphologyValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id]
  ): DataQualityValidator[TumorMorphology] = {

    case morph @ TumorMorphology(TumorMorphology.Id(id),patient,specimen,icdO3M,notes) =>

      (
        (patient must be (validReference[Patient.Id](Location("TumorMorphology",id,"patient")))),

        specimen must be (validReference(specimens)(Location("TumorMorphology",id,"specimen"))),
        
        icdO3M.validate
      )
      .mapN {case _: Product => morph }
      
  }


  implicit def histologyReportValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id]
  ): DataQualityValidator[HistologyReport] = {

    case histo @ HistologyReport(HistologyReport.Id(id),patient,specimen,date,morphology,tumorContent) =>

      (
        (patient must be (validReference[Patient.Id](Location("HistologyReport",id,"patient")))),

        specimen must be (validReference(specimens)(Location("HistologyReport",id,"specimen"))),

        (date mustBe defined otherwise (Error("Missing issue date") at Location("HistologyReport",id,"issuedOn"))),

        (morphology ifUndefined (Error("Missing TumorMorphology") at Location("HistologyReport",id,"tumorMorphology")))
          andThen (_ validate ),

        (tumorContent ifUndefined (Error("Missing TumorCellContent") at Location("HistologyReport",id,"tumorCellContent")))
          .andThen ( tc =>
            (
              tc.method must equal (TumorCellContent.Method.Histologic)
                otherwise (Error(s"Expected TumorCellContent method ${TumorCellContent.Method.Histologic}")
                  at Location("HistologyReport",id,"tumorContent")),
    
              tc validate
            )
            .mapN { case _: Product => tc }
          ),
      )
      .mapN { case _: Product => histo }

  }


  implicit def molecularPathologyValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id]
  ): DataQualityValidator[MolecularPathologyFinding] = {

    case molPath @ MolecularPathologyFinding(MolecularPathologyFinding.Id(id),patient,specimen,_,date,_) =>

      (
        (patient must be (validReference[Patient.Id](Location("MolecularPathologyFinding",id,"patient")))),

        specimen must be (validReference(specimens)(Location("MolecularPathologyFinding",id,"specimen"))),

        (date mustBe defined otherwise (Error("Missing issue date") at Location("MolecularPathologyFinding",id,"issuedOn"))),

      )
      .mapN { case _: Product => molPath }

  }


  private val hgncCatalog = HGNCCatalog.getInstance.get

  import scala.language.implicitConversions

  implicit def toHGNCGeneSymbol(gene: Variant.Gene): HGNCGene.Symbol =
    HGNCGene.Symbol(gene.value)

  private def validGeneSymbol(
    location: => Location
  ): DataQualityValidator[Variant.Gene] = 
    symbol => 
      (hgncCatalog.geneWithSymbol(symbol) mustBe defined otherwise 
        (Error(s"Invalid Gene Symbol ${symbol.value}") at location))
        .map(_ => symbol)
  


  implicit def ngsReportValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id],
  ): DataQualityValidator[SomaticNGSReport] = {


    case ngs @
      SomaticNGSReport(SomaticNGSReport.Id(id),patient,specimen,date,_,_,tumorContent,brcaness,msi,tmb,optSnvs,_,_,_,_) => {

      import SomaticNGSReport._

      val brcanessRange = ClosedInterval(0.0 -> 1.0)
      val msiRange      = ClosedInterval(0.0 -> 2.0)
      val tmbRange      = ClosedInterval(0.0 -> 1e6)  // TMB in mut/MBase, so [0,1000000]

      (
        (patient must be (validReference[Patient.Id](Location("SomaticNGSReport",id,"patient")))),

        specimen must be (validReference(specimens)(Location("SomaticNGSReport",id,"specimen"))),

        tumorContent.method must equal (TumorCellContent.Method.Bioinformatic)
          otherwise (Error(s"Expected TumorCellContent method '${TumorCellContent.Method.Bioinformatic}'")
            at Location("SomaticNGSReport",id,"tumorContent")),

        (tumorContent validate),
       
        (brcaness shouldBe defined otherwise (Info("Missing BRCAness value") at Location("SomaticNGSReport",id,"brcaness")))
          .andThen(
            opt =>
              opt.get.value must be (in (brcanessRange)) otherwise (
                  Error(s"BRCAness value '${opt.get.value}' not in reference range $brcanessRange")
                    at Location("SomaticNGSReport",id,"brcaness")
                )
              ),
             
        (msi shouldBe defined otherwise (Info("Missing MSI value") at Location("SomaticNGSReport",id,"msi")))
          .andThen(
            opt =>
              opt.get.value must be (in (msiRange)) otherwise (
                Error(s"MSI value '${opt.get.value}' not in reference range $msiRange") at Location("SomaticNGSReport",id,"msi")
              )
            ),
             
        tmb.value must be (in (tmbRange))
          otherwise (Error(s"TMB value '${tmb.value}' not in reference range $tmbRange") at Location("SomaticNGSReport",id,"tmb")),             

        optSnvs.fold(
          List.empty[SimpleVariant].validNel[Issue]
        )(
          _ validateEach (
              snv => 
                (snv.gene.code must be (validGeneSymbol(Location("SomaticNGSReport",id,s"SimpleVariant/${snv.id.value}"))))
                  .map(_ => snv)
            )
        )

        //TODO: validate other variants, at least gene symbols
      )
      .mapN { case _: Product => ngs }

    }

  }



  implicit def carePlanValidator(
    implicit
    patId: Patient.Id,
    diagnosisRefs: List[Diagnosis.Id],
    recommendationRefs: Seq[TherapyRecommendation.Id],
    counsellingRequestRefs: Seq[GeneticCounsellingRequest.Id],
    rebiopsyRequestRefs: Seq[RebiopsyRequest.Id]
  ): DataQualityValidator[CarePlan] = {

    case cp @ CarePlan(CarePlan.Id(id),patient,diag,date,_,noTarget,recommendations,counsellingReq,rebiopsyRequests) =>

      (
        (patient must be (validReference[Patient.Id](Location("CarePlan",id,"patient")))),

        diag must be (validReference(diagnosisRefs)(Location("CarePlan",id,"diagnosis"))),

        (date shouldBe defined otherwise (Warning("Missing Recording Date") at Location("CarePlan",id,"issuedOn"))),

        recommendations mustBe defined otherwise (
          Error("Missing Therapy Recommendations") at Location("CarePlan",id,"recommendations"))
          andThen (
            _.get must be (validReferences[TherapyRecommendation.Id](Location("CarePlan",id,"recommendations")))
          ),

        counsellingReq
          .map(_ must be (validReference(counsellingRequestRefs)(Location("CarePlan",id,"geneticCounsellingRequest"))))
          .getOrElse(None.validNel[Issue]),
 
        rebiopsyRequests
          .map(_ must be (validReferences[RebiopsyRequest.Id](Location("CarePlan",id,"rebiopsyRequests"))))
          .getOrElse(List.empty[RebiopsyRequest.Id].validNel[Issue]) 
      )
      .mapN { case _: Product => cp }

  }

 
  implicit def recommendationValidator(
    implicit
    patId: Patient.Id,
    diagnosisRefs: List[Diagnosis.Id],
    ngsReports: List[SomaticNGSReport]
  ): DataQualityValidator[TherapyRecommendation] = {

    case rec @ TherapyRecommendation(TherapyRecommendation.Id(id),patient,diag,date,medication,priority,loe,optNgsId,supportingVariants) =>

      (
        (patient must be (validReference[Patient.Id](Location("TherapyRecommendation",id,"patient")))),

        diag must be (validReference(diagnosisRefs)(Location("TherapyRecommendation",id,"diagnosis"))),

        (date shouldBe defined otherwise (Warning("Missing Recording Date") at Location("TherapyRecommendation",id,"issuedOn"))),

        (medication validateEach),

        (priority shouldBe defined otherwise (Warning("Missing Priority") at Location("TherapyRecommendation",id,"priority"))),

        (loe shouldBe defined otherwise (Warning("Missing Level of Evidence") at Location("TherapyRecommendation",id,"levelOfEvidence"))),

        optNgsId mustBe defined otherwise (
          Error(s"Missing Reference to SomaticNGSReport") at Location("TherapyRecommendation",id,"ngsReport")
        ) andThen (
          ngsId =>
            ngsReports.find(_.id == ngsId.get) mustBe defined otherwise (
              Fatal(s"Invalid Reference to SomaticNGSReport/${ngsId.get.value}") at Location("TherapyRecommendation",id,"ngsReport")
            ) andThen (
              ngsReport =>

              supportingVariants shouldBe defined otherwise (
                Warning("Missing Supporting Variants") at Location("TherapyRecommendation",id,"supportingVariants")
              ) andThen (refs =>
                refs.get must be (validReferences(Location("TherapyRecommendation",id,"supportingVariants"))(ngsReport.get.variants.map(_.id))) 
              )
            )
        )
      )
      .mapN { case _: Product => rec }

  }


  implicit def counsellingRequestValidator(
    implicit
    patId: Patient.Id
  ): DataQualityValidator[GeneticCounsellingRequest] = {

    case req @ GeneticCounsellingRequest(GeneticCounsellingRequest.Id(id),patient,date,_) =>

      (
        (patient must be (validReference[Patient.Id](Location("GeneticCounsellingRequest",id,"patient")))),

        (date shouldBe defined otherwise (Warning("Missing Recording Date") at Location("GeneticCounsellingRequest",id,"issuedOn"))),
      )
      .mapN { case _: Product => req }

  }


  implicit def rebiopsyRequestValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id]
  ): DataQualityValidator[RebiopsyRequest] = {

    case req @ RebiopsyRequest(RebiopsyRequest.Id(id),patient,specimen,date) =>

      (
        (patient must be (validReference[Patient.Id](Location("RebiopsyRequest",id,"patient")))),

        (date shouldBe defined otherwise (Warning("Missing Recording Date") at Location("RebiopsyRequest",id,"issuedOn"))),

        specimen must be (validReference(specimens)(Location("RebiopsyRequest",id,"specimen"))),
      )
      .mapN { case _: Product => req }

  }


  implicit def histologyReevaluationRequestValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id]
  ): DataQualityValidator[HistologyReevaluationRequest] = {

    case req @ HistologyReevaluationRequest(HistologyReevaluationRequest.Id(id),patient,specimen,date) =>

      (
        (patient must be (validReference[Patient.Id](Location("HistologyReevaluationRequest",id,"patient")))),

        (date shouldBe defined otherwise (Warning("Missing Recording Date") at Location("HistologyReevaluationRequest",id,"issuedOn"))),

        specimen must be (validReference(specimens)(Location("HistologyReevaluationRequest",id,"specimen"))),
      )
      .mapN { case _: Product => req }

  }



  private val nctNumRegex = """(NCT\d{8})""".r

  implicit def studyInclusionRequestValidator(
    implicit
    patId: Patient.Id,
    specimens: Seq[Specimen.Id]
  ): DataQualityValidator[StudyInclusionRequest] = {

    case req @ StudyInclusionRequest(StudyInclusionRequest.Id(id),patient,diag,NCTNumber(nct),date) =>

      (
        (patient must be (validReference[Patient.Id](Location("StudyInclusionRequest",id,"patient")))),

        (nct must matchRegex (nctNumRegex) otherwise (
          Error(s"Invalid NCT Number pattern '${nct}'") at Location("StudyInclusionRequest",id,"nctNumber"))),  

        (date shouldBe defined otherwise (Warning("Missing Recording Date") at Location("StudyInclusionRequest",id,"issuedOn"))),

      )
      .mapN { case _: Product => req }

  }



  implicit def claimValidator(
    implicit
    patId: Patient.Id,
    recommendationRefs: Seq[TherapyRecommendation.Id],
  ): DataQualityValidator[Claim] = {

    case cl @ Claim(Claim.Id(id),patient,_,therapy) =>

      (
        (patient must be (validReference[Patient.Id](Location("Claim",id,"patient")))),

        therapy must be (validReference(recommendationRefs)(Location("Claim",id,"specimen"))),
      )
      .mapN { case _: Product => cl }

  }


  implicit def claimResponseValidator(
    implicit
    patId: Patient.Id,
    claimRefs: Seq[Claim.Id],
  ): DataQualityValidator[ClaimResponse] = {

    case cl @ ClaimResponse(ClaimResponse.Id(id),claim,patient,_,status,reason) =>

      (
        patient must be (validReference[Patient.Id](Location("ClaimResponse",id,"patient"))),

        claim must be (validReference(claimRefs)(Location("ClaimResponse",id,"claim"))),

        if (status == ClaimResponse.Status.Rejected)
          reason shouldBe defined otherwise (
            Warning("Missing Reason for Rejected ClaimResponse") at Location("ClaimResponse",id,"reason")
          )
        else 
          reason.validNel[Issue]
      )
      .mapN { case _: Product => cl }

  }


  implicit def molecularTherapyValidator(
    implicit
    patId: Patient.Id,
    recommendationRefs: Seq[TherapyRecommendation.Id]
  ): DataQualityValidator[MolecularTherapy] = {

    case th @ NotDoneTherapy(TherapyId(id),patient,recordedOn,basedOn,notDoneReason,note) =>

      (
        patient must be (validReference[Patient.Id](Location("MolecularTherapy",id,"patient"))),

        basedOn must be (validReference(recommendationRefs)(Location("MolecularTherapy",id,"basedOn")))
      )
      .mapN { case _: Product => th }


    case th @ StoppedTherapy(TherapyId(id),patient,_,basedOn,_,medication,_,_,_) =>

      (
        patient must be (validReference[Patient.Id](Location("MolecularTherapy",id,"patient"))),

        basedOn must be (validReference(recommendationRefs)(Location("MolecularTherapy",id,"basedOn"))),

        medication.toList.validateEach
      )
      .mapN { case _: Product => th }


    case th @ CompletedTherapy(TherapyId(id),patient,_,basedOn,_,medication,_,_) =>

      (
        patient must be (validReference[Patient.Id](Location("MolecularTherapy",id,"patient"))),

        basedOn must be (validReference(recommendationRefs)(Location("MolecularTherapy",id,"basedOn"))),

        medication.toList.validateEach
      )
      .mapN { case _: Product => th }


    case th @ OngoingTherapy(TherapyId(id),patient,_,basedOn,_,medication,_,_) =>

      (
        patient must be (validReference[Patient.Id](Location("MolecularTherapy",id,"patient"))),

        basedOn must be (validReference(recommendationRefs)(Location("MolecularTherapy",id,"basedOn"))),

        medication.toList.validateEach
      )
      .mapN { case _: Product => th }

  }


  implicit def reponseValidator(
    implicit
    patId: Patient.Id,
    therapyRefs: Seq[TherapyId]
  ): DataQualityValidator[Response] = {

    case resp @ Response(Response.Id(id),patient,therapy,_,_) =>
      (
        (patient must be (validReference[Patient.Id](Location("Response",id,"patient")))),

        (therapy must be (validReference(therapyRefs)(Location("Response",id,"therapy"))))
      )
      .mapN{ case _: Product => resp }

  }



  implicit val mtbFileValidator: DataQualityValidator[MTBFile] = {

    case mtbfile @ MTBFile(
      patient,
      consent,
      episode,
      diagnoses,
      familyMemberDiagnoses,
      previousGuidelineTherapies,
      lastGuidelineTherapy,
      ecogStatus,
      specimens,
      molPathoFindings,
      histologyReports,
      ngsReports,
      carePlans,
      recommendations,
      counsellingRequests,
      rebiopsyRequests,
      histologyReevaluationRequests,
      studyInclusionRequests,
      claims,
      claimResponses,
      molecularTherapies,
      responses
    ) =>

    implicit val patId = patient.id  

    consent.status match {

      case Consent.Status.Rejected => {
        (
          patient.validate,

          consent.validate,

          episode.validate,

          diagnoses mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"diagnoses")),

          familyMemberDiagnoses mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"familyMemberDiagnoses")),

          previousGuidelineTherapies mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"previousGuidelineTherapies")),

          lastGuidelineTherapy mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"lastGuidelineTherapy")),

          ecogStatus mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"ecogStatus")),

          specimens mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"specimens")),

          histologyReports mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"histologyReports")),

          ngsReports mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"ngsReports")),

          carePlans mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"carePlans")),

          recommendations mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"recommendations")),

          counsellingRequests mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"counsellingRequests")),

          rebiopsyRequests mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"rebiopsyRequests")),

          claims mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"claims")),

          claimResponses mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"claimResponses")),

          molecularTherapies mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"molecularTherapies")),

          responses mustBe undefined otherwise (
            Fatal(s"Data must not be defined for Consent '${consent.status}'")
              at Location("MTBFile",patId.value,"responses")),
        )
        .mapN { case _: Product => mtbfile }
      }



      case Consent.Status.Active => {
        
        implicit val diagnosisRefs =
          diagnoses.getOrElse(List.empty[Diagnosis])
            .map(_.id)

        implicit val icd10codes =
          diagnoses.getOrElse(List.empty[Diagnosis])
            .map(_.icd10)
            .filter(_.isDefined)
            .map(_.get.code)
  
        implicit val histoRefs =
          histologyReports.getOrElse(List.empty[HistologyReport]).map(_.id)
  
        implicit val specimenRefs =
          specimens.getOrElse(List.empty[Specimen]).map(_.id)
  
        implicit val allNgsReports =
          ngsReports.getOrElse(List.empty[SomaticNGSReport])
  
        implicit val recommendationRefs =
          recommendations.getOrElse(List.empty[TherapyRecommendation]).map(_.id)
  
        implicit val counsellingRequestRefs =
          counsellingRequests.getOrElse(List.empty[GeneticCounsellingRequest]).map(_.id)
  
        implicit val rebiopsyRequestRefs =
          rebiopsyRequests.getOrElse(List.empty[RebiopsyRequest]).map(_.id)
  
        implicit val claimRefs =
          claims.getOrElse(List.empty[Claim]).map(_.id)
 
        // Get List of TherapyIds as combined IDs of Previous and Last Guideline Therapies and Molecular Therapies
        implicit val therapyRefs =
          previousGuidelineTherapies.map(_.map(_.id)).getOrElse(List.empty[TherapyId]) ++
          lastGuidelineTherapy.map(_.id) ++
          molecularTherapies.map(_.flatMap(_.history.map(_.id))).getOrElse(List.empty[TherapyId])
  
        (
          patient.validate,
          consent.validate,
          episode.validate,
  
          (diagnoses ifUndefined (Error("Missing diagnosis records") at Location("MTBFile",patId.value,"diagnoses")))
            andThen (_ ifEmpty (Error("Missing diagnoses records") at Location("MTBFile",patId.value,"diagnoses")))
            andThen (_ validateEach),
  
          familyMemberDiagnoses
            .map(_ validateEach)
            .getOrElse(List.empty[FamilyMemberDiagnosis].validNel[Issue]),


          (previousGuidelineTherapies ifUndefined (Warning("Missing previous Guideline Therapies") at Location("MTBFile",patId.value,"previousGuidelineTherapies")))
            andThen (_ ifEmpty (Warning("Missing previous Guideline Therapies") at Location("MTBFile",patId.value,"previousGuidelineTherapies")))
            andThen (_ validateEach),
  
          (lastGuidelineTherapy ifUndefined (Error("Missing last Guideline Therapy") at Location("MTBFile",patId.value,"lastGuidelineTherapies")))
            andThen (_ validate),
  
          (ecogStatus ifUndefined (Warning("Missing ECOG Performance Status records") at Location("MTBFile",patId.value,"ecogStatus")))
            andThen (_ ifEmpty (Warning("Missing ECOG Performance Status records") at Location("MTBFile",patId.value,"ecogStatus")))
            andThen (_ validateEach),
  
          (specimens ifUndefined (Warning("Missing Specimen records") at Location("MTBFile",patId.value,"specimens")))
            andThen (_ ifEmpty (Warning("Missing Specimen records") at Location("MTBFile",patId.value,"specimens")))
            andThen (_ validateEach),
  
          (histologyReports ifUndefined (Warning("Missing HistologyReport records") at Location("MTBFile",patId.value,"histologyReports")))
            andThen (_ ifEmpty (Warning("Missing HistologyReport records") at Location("MTBFile",patId.value,"histologyReports")))
            andThen (_ validateEach),
  
          (molPathoFindings ifUndefined (Warning("Missing MolecularPathology records") at Location("MTBFile",patId.value,"molecularPathologyFindings")))
            andThen (_ ifEmpty (Warning("Missing MolecularPathology records") at Location("MTBFile",patId.value,"molecularPathologyFindings")))
            andThen (_ validateEach),
  
          (ngsReports ifUndefined (Warning("Missing SomaticNGSReport records") at Location("MTBFile",patId.value,"ngsReports")))
            andThen (_ ifEmpty (Warning("Missing SomaticNGSReport records") at Location("MTBFile",patId.value,"ngsReports")))
            andThen (_ validateEach),
  
          (carePlans ifUndefined (Warning("Missing CarePlan records") at Location("MTBFile",patId.value,"carePlans")))
            andThen (_ ifEmpty (Warning("Missing CarePlan records") at Location("MTBFile",patId.value,"carePlans")))
            andThen (_ validateEach),
  
          (recommendations ifUndefined (Warning("Missing TherapyRecommendation records") at Location("MTBFile",patId.value,"recommendations")))
            andThen (_ ifEmpty (Warning("Missing TherapyRecommendation records") at Location("MTBFile",patId.value,"recommendations")))
            andThen (_ validateEach),
  
          counsellingRequests.map(_ validateEach)
            .getOrElse(List.empty[GeneticCounsellingRequest].validNel[Issue]),
  
          rebiopsyRequests.map(_ validateEach)
            .getOrElse(List.empty[RebiopsyRequest].validNel[Issue]),
  
          histologyReevaluationRequests.map(_ validateEach)
            .getOrElse(List.empty[HistologyReevaluationRequest].validNel[Issue]),
  
          studyInclusionRequests.map(_ validateEach)
            .getOrElse(List.empty[StudyInclusionRequest].validNel[Issue]),
  
          (claims ifUndefined (Warning("Missing Insurance Claim records") at Location("MTBFile",patId.value,"claims")))
            andThen (_ ifEmpty (Warning("Missing Insurance Claim records") at Location("MTBFile",patId.value,"claims")))
            andThen (_ validateEach),
  
          (claimResponses ifUndefined (Warning("Missing ClaimResponse records") at Location("MTBFile",patId.value,"claimResponses")))
            andThen (_ ifEmpty (Warning("Missing ClaimResponse records") at Location("MTBFile",patId.value,"claimResponses")))
            andThen (_ validateEach),
  
          (molecularTherapies ifUndefined (Warning("Missing MolecularTherapy records") at Location("MTBFile",patId.value,"molecularTherapies")))
            andThen (_ ifEmpty (Warning("Missing MolecularTherapy records") at Location("MTBFile",patId.value,"molecularTherapies")))
            andThen (_.flatMap(_.history) validateEach),
  
          //TODO: validate Responses
  
        )
        .mapN { case _: Product => mtbfile }

      }

    }

  }

}
