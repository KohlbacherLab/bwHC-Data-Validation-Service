package de.bwhc.mtb.data.entry.views


import java.time.{
  Instant,
  LocalDate,
  LocalDateTime
}
import java.time.temporal.Temporal
import java.time.format.DateTimeFormatter
import DateTimeFormatter.{
  ISO_LOCAL_DATE,
  ISO_LOCAL_DATE_TIME,
  ISO_INSTANT
}


import cats.Id
import cats.data.NonEmptyList
import cats.syntax.either._

import de.bwhc.util.num._

import de.bwhc.mtb.data.entry.dtos._

import de.bwhc.catalogs.icd
import de.bwhc.catalogs.icd._
import de.bwhc.catalogs.hgnc.{HGNCGene,HGNCCatalog}
import de.bwhc.catalogs.med
import de.bwhc.catalogs.med.MedicationCatalog


trait mappings
{

  import ValueSets._


  implicit val icd10gmCatalog: ICD10GMCatalogs

  implicit val icdO3Catalog: ICDO3Catalogs

  implicit val medicationCatalog: MedicationCatalog

  implicit val hgncCatalog: HGNCCatalog[Id]  
//  implicit val hgncCatalog: HGNCCatalog  



  implicit class MappingOps[T](val t: T)
  {
    def mapTo[V](implicit f: T => V) = f(t)
  }


  implicit class TemporalFormattingOps[T <: Temporal](val t: T)
  {
     private val ddMMyyyy = DateTimeFormatter.ofPattern("dd.MM.yyyy")

     def toISOFormat: String = {
       t match {
         case ld:  LocalDate     => ISO_LOCAL_DATE.format(ld)
         case ldt: LocalDateTime => ISO_LOCAL_DATE_TIME.format(ldt)
         case t:   Instant       => ISO_INSTANT.format(t)
       }
     }

     def toGermanDate: String = {
       ddMMyyyy.format(t)
     }

  }


  implicit val patientToView: ((Patient,Consent,MTBEpisode)) => PatientView = {
    case (pat,consent,episode) =>
      PatientView(
        pat.id,
        ValueSet[Gender.Value].displayOf(pat.gender).get,
        pat.birthDate.toRight(NotAvailable),
        pat.managingZPM.toRight(NotAvailable),
        pat.insurance.toRight(NotAvailable),
        pat.dateOfDeath.toRight(NotAvailable),
        consent.status,
        episode.period.start
      )
  }


  implicit def periodToDisplay[T <: Temporal, P <: Period[T]]: P => PeriodDisplay[T] = {

    case OpenEndPeriod(start,end) =>
      PeriodDisplay(s"${start.toISOFormat} - ${end.map(_.toISOFormat).getOrElse("N/A")}")

    case ClosedPeriod(start,end) =>
      PeriodDisplay(s"${start.toISOFormat} - ${end.toISOFormat}")

  }



  implicit def icd10ToDisplay(
    implicit icd10gm: ICD10GMCatalogs
  ): Coding[ICD10GM] => ICD10Display = {
     icd10 =>
       icd10gm.code(
         icd.ICD10GM.Code(icd10.code.value),
       )
       .map(c => ICD10Display(s"${c.code.value}: ${c.display.get}"))
       .getOrElse(ICD10Display(s"${icd10.code.value}: ${icd10.display.getOrElse("N/A")}"))
  }


  implicit def icdO3TtoDisplay(
    implicit icdO3: ICDO3Catalogs
  ): Coding[ICDO3T] => ICDO3TDisplay = {
     icdO3T =>
      icdO3.topographyCodings()
        .find(_.code == icd.ICDO3.TopographyCode(icdO3T.code.value))
        .map(c => ICDO3TDisplay(s"${c.code.value}: ${c.display}"))
        .getOrElse(ICDO3TDisplay(s"${icdO3T.code.value}: ${icdO3T.display.getOrElse("N/A")}"))
  }


  implicit def icdO3MtoDisplay(
    implicit icdO3: ICDO3Catalogs
  ): Coding[ICDO3M] => ICDO3MDisplay = {
    icdO3M =>
      icdO3.morphologyCodings()
        .find(_.code == icd.ICDO3.MorphologyCode(icdO3M.code.value))
        .map(c => ICDO3MDisplay(s"${c.code.value}: ${c.display}"))
        .getOrElse(ICDO3MDisplay(s"${icdO3M.code.value}: ${icdO3M.display.getOrElse("N/A")}"))
  }


/*
  implicit def medicationToDisplay(
    implicit medications: MedicationCatalog
  ): List[Coding[Medication]] => MedicationDisplay = {
    meds =>
      MedicationDisplay(
        meds.map(
          m =>
            medications
              .findByCode(med.Medication.Code(m.code.value))
              .map(c => s"${c.name.get} (${c.code.value})")
              .getOrElse(s"${m.display.getOrElse("N/A")} (${m.code.value})")
        )
        .reduceLeftOption(_ + ", " + _)
        .getOrElse("N/A")
      )
  }
*/

  implicit def medicationCodingsToDisplay(
    implicit medications: MedicationCatalog
  ): List[Medication.Coding] => MedicationDisplay = {
    meds =>
      MedicationDisplay(
        meds.map(
          m =>
            medications
              .findWithCode(m.code.value)
              .map(c => s"${c.name} (${c.code.value})")
              .getOrElse(s"${m.display.getOrElse("N/A")} (${m.code.value})")
        )
        .reduceLeftOption(_ + ", " + _)
        .getOrElse("N/A")
      )
  }


  implicit val diagnosisToView: Diagnosis => DiagnosisView = {
    diag => 
      DiagnosisView(
        diag.id,
        diag.patient,
        diag.recordedOn.toRight(NotAvailable),
        diag.icd10.map(_.mapTo[ICD10Display]).toRight(NotAvailable),
        diag.icdO3T.map(_.mapTo[ICDO3TDisplay]).toRight(NotAvailable),
        diag.whoGrade
          .map(_.code)
          .flatMap(
            c =>
              ValueSet[WHOGrade.Value].displayOf(c)
                .map(d => WHOGradeDisplay(s"${c}: ${d}"))
          )
          .toRight(NotAvailable),
        diag.statusHistory
          .filterNot(_.isEmpty)
          .map(_.sortWith((t1,t2) => t1.date isBefore t2.date))
          .flatMap {
            _.map{
              case Diagnosis.StatusOnDate(status,date) =>
                s"${date.toISOFormat}: ${ValueSet[Diagnosis.Status.Value].displayOf(status).get}"
            }
            .reduceLeftOption(_ + ", " + _)
          } 
          .toRight(NotAvailable),
        diag.guidelineTreatmentStatus
          .flatMap(ValueSet[GuidelineTreatmentStatus.Value].displayOf)
          .toRight(NotAvailable)

      )
  }


  implicit val famMemDiagnosisToView: FamilyMemberDiagnosis => FamilyMemberDiagnosisView = {
    fmdiag =>
      FamilyMemberDiagnosisView(
        fmdiag.id,
        fmdiag.patient,
        ValueSet[FamilyMember.Relationship.Value]
          .displayOf(fmdiag.relationship.code)
          .get
      )
  }



  implicit val responseToDisplay: Response => ResponseDisplay = {
    resp =>
      ValueSet[RECIST.Value]
        .displayOf(resp.value.code)
        .map(ResponseDisplay(_))
        .get
  }


  implicit def guidelineTherapyToView[T <: GuidelineTherapy]:
    ((
     T,
     Option[Diagnosis],
     Option[Response])) => GuidelineTherapyView = {

    case (therapy,diagnosis,response) =>

      therapy match { 
      
        case th: PreviousGuidelineTherapy =>
          GuidelineTherapyView(
            th.id,
            th.patient,
            diagnosis.flatMap(_.icd10.map(_.mapTo[ICD10Display])).toRight(NotAvailable),
            th.therapyLine.toRight(NotAvailable),
            NotAvailable.asLeft[PeriodDisplay[LocalDate]],
            th.medication.map(_.mapTo[MedicationDisplay]).toRight(NotAvailable),
            NotAvailable.asLeft[String],
            response.map(_.mapTo[ResponseDisplay]).toRight(NotAvailable),
            response.filter(_.value.code == RECIST.PD).map(_.effectiveDate).toRight(Undefined),
          )
        
        case th: LastGuidelineTherapy =>
          GuidelineTherapyView(
            th.id,
            th.patient,
            diagnosis.flatMap(_.icd10.map(_.mapTo[ICD10Display])).toRight(NotAvailable),
            th.therapyLine.toRight(NotAvailable),
            th.period.map(_.mapTo[PeriodDisplay[LocalDate]]).toRight(NotAvailable),
            th.medication.map(_.mapTo[MedicationDisplay]).toRight(NotAvailable),
            th.reasonStopped
              .flatMap(c => ValueSet[GuidelineTherapy.StopReason.Value].displayOf(c.code))
              .toRight(NotAvailable),
            response.map(_.mapTo[ResponseDisplay]).toRight(NotAvailable),
            response.filter(_.value.code == RECIST.PD).map(_.effectiveDate).toRight(Undefined),
          )
      
      }

  }



  implicit val ecogToDisplay: Coding[ECOG.Value] => ECOGDisplay = {
    ecog =>
      ValueSet[ECOG.Value]
        .displayOf(ecog.code)
        .map(ECOGDisplay(_))
        .get  // safe to call 
  }

  implicit val ecogsToDisplay: ((Patient,List[ECOGStatus])) => ECOGStatusView = {
    case (patient,ecogs) =>
      ECOGStatusView(
        patient.id,
        ecogs.map(ecog =>
          DatedValue(
            ecog.effectiveDate.map(_.toISOFormat).getOrElse("N/A"),
            ecog.value.mapTo[ECOGDisplay]
          )
        )
      )
  }



  implicit val specimenToView: Specimen => SpecimenView = {
    specimen =>
      SpecimenView(
        specimen.id,
        specimen.patient,
        specimen.icd10.mapTo[ICD10Display],
        specimen.`type`.flatMap(ValueSet[Specimen.Type.Value].displayOf)
          .toRight(NotAvailable),
        specimen.collection.map(_.date).toRight(NotAvailable),
        specimen.collection.map(_.localization)
          .flatMap(ValueSet[Specimen.Collection.Localization.Value].displayOf)
          .toRight(NotAvailable),
        specimen.collection.map(_.method)
          .flatMap(ValueSet[Specimen.Collection.Method.Value].displayOf)
          .toRight(NotAvailable),
      )
  }


  implicit val molPathoToView: MolecularPathologyFinding => MolecularPathologyFindingView = {
    finding =>
      MolecularPathologyFindingView(
        finding.id,
        finding.patient,
        finding.specimen,
        finding.performingInstitute.toRight(NotAvailable),
        finding.issuedOn.toRight(NotAvailable),
        finding.note
      )
  }



  implicit val tumorCellContentToDisplay: TumorCellContent => TumorCellContentDisplay = {
    tc =>
//      val valuePercent = tc.value * 100
      val valuePercent = (tc.value * 100).toInt
      val method       = ValueSet[TumorCellContent.Method.Value].displayOf(tc.method).get

      TumorCellContentDisplay(s"$valuePercent % ($method)")
  }


  implicit val histologyReportToView: HistologyReport => HistologyReportView = {
    report =>
      HistologyReportView(
        report.id,
        report.patient,
        report.specimen,
        report.issuedOn.toRight(NotAvailable),
        report.tumorMorphology.map(_.value.mapTo[ICDO3MDisplay]).toRight(NotAvailable),
        report.tumorCellContent.map(_.mapTo[TumorCellContentDisplay]).toRight(NotAvailable),
        report.tumorMorphology.flatMap(_.note).getOrElse("-"),
      )
  }


  import SomaticNGSReport._
 

  implicit val tmbToDisplay: TMB => TMBDisplay = {
    tmb => TMBDisplay(s"${tmb.value} mut/MBase")   
//    tmb => TMBDisplay(s"${tmb.value.withDecimals(1)} mut/MBase")   
  }


  import Variant.{StartEnd, Gene}

  implicit val startEndToDisplay: StartEnd => StartEndDisplay = {
    case StartEnd(start,optEnd) =>
      optEnd.map(
        end => StartEndDisplay(s"${start} - $end")
      )
      .getOrElse(  
        StartEndDisplay(start.toString)
      )
  }


  implicit def geneCodingToDisplay(
    implicit hgnc: HGNCCatalog[cats.Id]
//    implicit hgnc: HGNCCatalog
  ): Coding[Gene] => GeneDisplay = {
    c =>
/*
      hgncCatalog
        .gene(HGNCGene.Id(c.code.value))
        .orElse(
          hgncCatalog.geneWithSymbol(c.code.value)
            .headOption   //TODO: re-consider this use of first hit; maybe point out ambiguity??
        )
        .map(g => s"${g.symbol}: ${g.name}")
        .map(GeneDisplay(_))
        .getOrElse(GeneDisplay(s"${c.code.value}: ${c.display.getOrElse("-")}"))
*/
      hgncCatalog
        .geneWithSymbol(c.code.value)
        .headOption   //TODO: re-consider this use of first hit; maybe point out ambiguity??
        .map(g => s"${g.symbol}: ${g.name}")
        .map(GeneDisplay(_))
        .getOrElse(GeneDisplay(s"${c.code.value}: ${c.display.getOrElse("-")}"))

  }


  implicit def genesToDisplay: List[Gene] => Option[GeneDisplay] = {
    genes =>
      genes.map(_.value)
        .reduceLeftOption(_ + ", " + _)
        .map(GeneDisplay(_))
  }


  implicit val simpleVariantToView: SimpleVariant => SimpleVariantView = {
    sv =>
      SimpleVariantView(
        sv.chromosome,
        sv.gene.map(_.mapTo[GeneDisplay]).toRight(NotAvailable),
        sv.startEnd.mapTo[StartEndDisplay],
        sv.refAllele,
        sv.altAllele,
        sv.functionalAnnotation.map(_.code).toRight(NotAvailable),
        sv.dnaChange.map(_.code).toRight(Undefined),
        sv.aminoAcidChange.map(_.code).toRight(Undefined),
        sv.readDepth,
        sv.allelicFrequency,
        sv.cosmicId.toRight(NotAvailable),
        sv.dbSNPId.toRight(NotAvailable),
        sv.interpretation.code
      )
  }


  implicit val cnvToView: CNV => CNVView = {
    cnv =>
      CNVView(
        cnv.chromosome,
        cnv.startRange.mapTo[StartEndDisplay],
        cnv.endRange.mapTo[StartEndDisplay],
        cnv.totalCopyNumber,
        cnv.relativeCopyNumber,
        cnv.cnA.toRight(NotAvailable),
        cnv.cnB.toRight(NotAvailable),
        cnv.reportedAffectedGenes
          .map(_.map(_.code))
          .flatMap(_.mapTo[Option[GeneDisplay]])
          .toRight(NotAvailable),
        cnv.reportedFocality.toRight(NotAvailable),
        cnv.`type`,
        cnv.copyNumberNeutralLoH
          .map(_.map(_.code))
          .flatMap(_.mapTo[Option[GeneDisplay]])
          .toRight(NotAvailable)
      )
  }


  implicit val dnaFusionToView: DNAFusion => DNAFusionView = {
    case DNAFusion(
      _,
      DNAFusion.FunctionalDomain(chr5pr,pos5pr,gene5pr),
      DNAFusion.FunctionalDomain(chr3pr,pos3pr,gene3pr),
      numReads
    ) =>

      DNAFusionView(
        s"${gene5pr.code.value} :: ${gene3pr.code.value} (${chr5pr.value}:${pos5pr} :: ${chr3pr.value}:${pos3pr})",
        numReads
      )

  }

  implicit val rnaFusionToView: RNAFusion => RNAFusionView = {
    case RNAFusion(
      _,
      RNAFusion.FunctionalDomain(gene5pr,transcript5pr,exon5pr,pos5pr,strand5pr),
      RNAFusion.FunctionalDomain(gene3pr,transcript3pr,exon3pr,pos3pr,strand3pr),
      effect,
      cosmicId,
      numReads
    ) =>

    RNAFusionView(
      s"${gene5pr.code.value} (${transcript5pr.value}: ${exon5pr.value}) :: ${gene3pr.code.value} (${transcript3pr.value}: ${exon3pr.value})",
      pos5pr,
      strand5pr,
      pos3pr,
      strand3pr,
      effect.toRight(NotAvailable),
      cosmicId.toRight(NotAvailable),
      numReads
    )
  }



  implicit val rnaSeqToView: RNASeq => RNASeqView = {
    rnaSeq =>
      RNASeqView(
        rnaSeq.entrezId,
        rnaSeq.ensemblId,
        rnaSeq.gene.code,
        rnaSeq.transcriptId,
        rnaSeq.fragmentsPerKilobaseMillion,
        rnaSeq.fromNGS,
        rnaSeq.tissueCorrectedExpression,
        rnaSeq.rawCounts,
        rnaSeq.librarySize,
        rnaSeq.cohortRanking.toRight(NotAvailable)
      )
  }

  implicit val ngsReportToView: SomaticNGSReport => NGSReportView = {
    report =>
      NGSReportView(
        report.id,
        report.patient,
        report.specimen,
        report.issueDate,
        report.sequencingType,
        report.metadata,
        report.tumorCellContent.mapTo[TumorCellContentDisplay],
        report.brcaness.toRight(NotAvailable),
        report.msi.toRight(NotAvailable),
        report.tmb.mapTo[TMBDisplay],
        report.simpleVariants.getOrElse(List.empty[SimpleVariant]).map(_.mapTo[SimpleVariantView]),
        report.copyNumberVariants.getOrElse(List.empty[CNV]).map(_.mapTo[CNVView]),
        report.dnaFusions.getOrElse(List.empty[DNAFusion]).map(_.mapTo[DNAFusionView]),
        report.rnaFusions.getOrElse(List.empty[RNAFusion]).map(_.mapTo[RNAFusionView]),
        report.rnaSeqs.getOrElse(List.empty[RNASeq]).map(_.mapTo[RNASeqView])
     )
  }



  implicit val levelOfEvidenceToDisplay: LevelOfEvidence => LevelOfEvidenceDisplay = {
    loe =>
      LevelOfEvidenceDisplay(
        s"${loe.grading.code}, Zusätze: ${loe.addendums.flatMap(_.map(_.code.toString).reduceOption(_ + ", " + _)).getOrElse("keine")}"
      )
  }


  implicit val supportingVariantToDisplay: Variant => SupportingVariantDisplay = {
    v => 
      val repr = v match {
        case snv: SimpleVariant =>
          s"SNV ${snv.gene.map(_.code.value).getOrElse("Gene undefined")} ${snv.dnaChange.map(_.code.value).getOrElse("cDNA change undefined")}"
//          s"SNV ${snv.gene.code.value} ${snv.dnaChange.code.value}"
      
        case cnv: CNV => {
          val genes =
            cnv.reportedAffectedGenes
               .flatMap(_.map(_.code.value).reduceOption(_ + ", " + _))
               .getOrElse("N/A")
      
          s"CNV [${genes}], ${cnv.`type`}"
        }
      
        case DNAFusion(_,dom5pr,dom3pr,_) =>
          s"DNA-Fusion ${dom5pr.gene.code.value} :: ${dom3pr.gene.code.value}"
      
        case RNAFusion(_,dom5pr,dom3pr,_,_,_) =>
          s"RNA-Fusion ${dom5pr.gene.code.value} :: ${dom3pr.gene.code.value}"
      
        case rnaSeq: RNASeq =>
          s"RNA-Seq ${rnaSeq.gene.code.value}"
      
      }

      SupportingVariantDisplay(repr)
  }



  implicit val recommendationToDisplay:
    ((
//     (TherapyRecommendation,ICD10Display),
     (TherapyRecommendation,NotAvailable Or ICD10Display),
     List[Variant]
    )) => TherapyRecommendationView = {


    case ((rec,icd10),variants) =>

      val supportingVariants = rec.supportingVariants.getOrElse(List.empty[Variant.Id])

      TherapyRecommendationView(
        rec.id,
        rec.patient,
        icd10,
        rec.medication.map(_.mapTo[MedicationDisplay]).toRight(NotAvailable),
        rec.priority.toRight(NotAvailable),
        rec.levelOfEvidence.map(_.mapTo[LevelOfEvidenceDisplay]).toRight(NotAvailable),
        variants.filter(v => supportingVariants contains v.id).map(_.mapTo[SupportingVariantDisplay])
      )
  }


  implicit val carePlanToDisplay:
    ((
     (CarePlan,
      Diagnosis,
      List[TherapyRecommendation],
      Option[StudyInclusionRequest],
      Option[GeneticCounsellingRequest]
     ),
     List[Variant]
    )) => CarePlanView = {

    case ((carePlan,diagnosis,recommendations,studyInclusionRequest,geneticCounsellingRequest),variants) =>

      val icd10 = diagnosis.icd10.map(_.mapTo[ICD10Display]).toRight(NotAvailable)

      CarePlanView(
        carePlan.id,
        carePlan.patient,
        icd10,
        carePlan.issuedOn.toRight(NotAvailable),
        carePlan.description.toRight(NotAvailable),
        geneticCounsellingRequest.map(_.reason).toRight(No),
        studyInclusionRequest.map(_.nctNumber).toRight(NotAvailable),
        carePlan.noTargetFinding.isDefined,
        recommendations.map(rec => ((rec,icd10),variants).mapTo[TherapyRecommendationView]),
        carePlan.rebiopsyRequests.toRight(NotAvailable),
      )

  }


  implicit val carePlanDataToDisplay:
    ((
     (List[CarePlan],
       List[Diagnosis],
       List[TherapyRecommendation],
       List[StudyInclusionRequest],
       List[GeneticCounsellingRequest]
     ),
     List[SomaticNGSReport]
    )) => List[CarePlanView] = {

      case ((carePlans,diagnoses,recommendations,studyInclusionReqs,geneticCounsellingReqs), ngsReports) =>

        carePlans.map {
          cp =>
            ((
              cp,
              diagnoses.find(_.id == cp.diagnosis).get,  // safe to call, because validation enforces referential integrity
              cp.recommendations.fold(List.empty[TherapyRecommendation])(recs => recommendations.filter(rec => recs contains rec.id)),
              cp.studyInclusionRequest
                .flatMap(reqId => studyInclusionReqs.find(_.id == reqId)),
              cp.geneticCounsellingRequest
                .flatMap(reqId => geneticCounsellingReqs.find(_.id == reqId))
             ),
             ngsReports.flatMap(_.variants)
           )
           .mapTo[CarePlanView]
        }

  }




  implicit val claimWithResponseToDisplay: ((Claim,Option[ClaimResponse])) => ClaimStatusView = {

    case (claim,response) =>

      ClaimStatusView(
        claim.id,
        claim.patient,
        claim.therapy,
        claim.issuedOn,
        response.map(_.issuedOn).toRight(NotAvailable),
        response.map(_.status)
          .flatMap(ValueSet[ClaimResponse.Status.Value].displayOf)
          .map(ClaimResponseStatusDisplay(_))
          .toRight(NotAvailable),
        response.flatMap(_.reason)
          .flatMap(ValueSet[ClaimResponse.Reason.Value].displayOf)
          .map(ClaimResponseReasonDisplay(_))
          .toRight(NotAvailable),
      )
  }

  implicit val claimsWithResponsesToDisplay: ((List[Claim],List[ClaimResponse])) => List[ClaimStatusView] = {
    case (claims,claimResponses) =>
      claims.map(
        cl => (cl,claimResponses.find(_.claim == cl.id)).mapTo[ClaimStatusView]
      )

  }



  implicit val molecularTherapyToView:
  ((
    MolecularTherapy,
    Option[Diagnosis],
    Option[Response])) => MolecularTherapyView = {

    case (molTh,diag,resp) =>

    val status   = ValueSet[MolecularTherapy.Status.Value].displayOf(molTh.status).get
    val note     = molTh.note.getOrElse("-")
    val icd10    = diag.flatMap(_.icd10.map(_.mapTo[ICD10Display])).toRight(NotAvailable)
    val response = resp.map(_.mapTo[ResponseDisplay]).toRight(NotAvailable)
    val progressionDate = resp.filter(_.value.code == RECIST.PD).map(_.effectiveDate).toRight(Undefined)

    molTh match { 
 
      case th: NotDoneTherapy => 
        MolecularTherapyView(
          th.id,
          th.patient,
          icd10,
          status,
          th.recordedOn,
          th.basedOn,
          NotAvailable.asLeft[PeriodDisplay[LocalDate]],
          ValueSet[MolecularTherapy.NotDoneReason.Value].displayOf(th.notDoneReason.code).toRight(NotAvailable),
          Undefined.asLeft[MedicationDisplay],
          Undefined.asLeft[String],
          NotAvailable.asLeft[Dosage.Value],
          note,
          response,
          progressionDate
        )
       
      case th: StoppedTherapy => 
        MolecularTherapyView(
          th.id,
          th.patient,
          icd10,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
          th.medication.map(_.mapTo[MedicationDisplay]).toRight(NotAvailable),
          ValueSet[MolecularTherapy.StopReason.Value].displayOf(th.reasonStopped.code).toRight(NotAvailable),
          th.dosage.toRight(NotAvailable),
          note,
          response,
          progressionDate
        )
        
      case th: CompletedTherapy => 
        MolecularTherapyView(
          th.id,
          th.patient,
          icd10,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
          th.medication.map(_.mapTo[MedicationDisplay]).toRight(NotAvailable),
          Undefined.asLeft[String],
          th.dosage.toRight(NotAvailable),
          note,
          response,
          progressionDate
        )
        
      case th: OngoingTherapy => 
        MolecularTherapyView(
          th.id,
          th.patient,
          icd10,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
          th.medication.map(_.mapTo[MedicationDisplay]).toRight(NotAvailable),
          Undefined.asLeft[String],
          th.dosage.toRight(NotAvailable),
          note,
          response,
          progressionDate
        )

    }

  }


  implicit val molecularTherapiesToView:
    ((
     List[MolecularTherapy],
     List[TherapyRecommendation],
     List[Diagnosis],
     List[Response]
     )) => List[MolecularTherapyView] = {

    case (therapies,recommendations,diagnoses,responses) =>

      val diagsByRec =
        recommendations.map(rec => (rec.id,diagnoses.find(_.id == rec.diagnosis))).toMap

      therapies.map(
        th =>
          (
           th,
           diagsByRec.get(th.basedOn).flatten,
           responses.find(_.therapy == th.id)
          )
          .mapTo[MolecularTherapyView]
      )
  }



  implicit val mtbFileToView: MTBFile => MTBFileView = {

    mtbfile =>

      val diagnoses = mtbfile.diagnoses.getOrElse(List.empty[Diagnosis])

      val responses = mtbfile.responses.getOrElse(List.empty[Response])

      val ngsReports = mtbfile.ngsReports.getOrElse(List.empty[SomaticNGSReport])


      MTBFileView(
        (mtbfile.patient,
         mtbfile.consent,
         mtbfile.episode).mapTo[PatientView],

        diagnoses.map(_.mapTo[DiagnosisView]),

        mtbfile.familyMemberDiagnoses.getOrElse(List.empty)
          .map(_.mapTo[FamilyMemberDiagnosisView]),

        mtbfile.previousGuidelineTherapies.getOrElse(List.empty)
          .map(
            th =>
             (th,
              diagnoses.find(_.id == th.diagnosis),
              responses.find(_.therapy == th.id)).mapTo[GuidelineTherapyView]
          ) ++
            mtbfile.lastGuidelineTherapy
              .map(
                th =>
                  (th,
                   diagnoses.find(_.id == th.diagnosis),
                   responses.find(_.therapy == th.id)).mapTo[GuidelineTherapyView]
              ),

        mtbfile.ecogStatus
          .map((mtbfile.patient,_).mapTo[ECOGStatusView]),

        mtbfile.specimens.getOrElse(List.empty).map(_.mapTo[SpecimenView]),

        mtbfile.molecularPathologyFindings.getOrElse(List.empty)
          .map(_.mapTo[MolecularPathologyFindingView]),

        mtbfile.histologyReports.getOrElse(List.empty)
          .map(_.mapTo[HistologyReportView]),

        ngsReports.map(_.mapTo[NGSReportView]),

        (
          (mtbfile.carePlans.getOrElse(List.empty),
          mtbfile.diagnoses.getOrElse(List.empty),
          mtbfile.recommendations.getOrElse(List.empty),
          mtbfile.studyInclusionRequests.getOrElse(List.empty),
          mtbfile.geneticCounsellingRequests.getOrElse(List.empty)),
          ngsReports
        )
        .mapTo[List[CarePlanView]],

        (
          mtbfile.claims.getOrElse(List.empty),
          mtbfile.claimResponses.getOrElse(List.empty)
        )
        .mapTo[List[ClaimStatusView]],

        (
          mtbfile.molecularTherapies.getOrElse(List.empty)
            .filterNot(_.history.isEmpty).map(_.history.head),
          mtbfile.recommendations.getOrElse(List.empty), 
          diagnoses,
          responses 
        )
        .mapTo[List[MolecularTherapyView]]

      )

  }




}

object mappings extends mappings
{

  implicit val icd10gmCatalog = ICD10GMCatalogs.getInstance.get

  implicit val icdO3Catalog   = ICDO3Catalogs.getInstance.get

  implicit val medicationCatalog = MedicationCatalog.getInstance.get

  implicit val hgncCatalog = HGNCCatalog.getInstance.get

}
