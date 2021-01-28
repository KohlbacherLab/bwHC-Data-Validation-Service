package de.bwhc.mtb.data.entry.views


import java.time.{
  Instant,
  LocalDate,
  LocalDateTime
}
import java.time.temporal.Temporal
import java.time.format.DateTimeFormatter.{
  ISO_LOCAL_DATE,
  ISO_LOCAL_DATE_TIME,
  ISO_INSTANT
}


import cats.syntax.either._

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

  implicit val hgncCatalog: HGNCCatalog  



  implicit class MappingOps[T](val t: T)
  {
    def mapTo[V](implicit f: T => V) = f(t)
  }


  implicit class TemporalFormattingOps[T <: Temporal](val t: T)
  {
     def toISOFormat: String = {
       t match {
         case ld:  LocalDate     => ISO_LOCAL_DATE.format(ld)
         case ldt: LocalDateTime => ISO_LOCAL_DATE_TIME.format(ldt)
         case t:   Instant       => ISO_INSTANT.format(t)
       }
     }

  }




  implicit val patientToView: Patient => PatientView = {
    pat =>
      PatientView(
        pat.id,
        ValueSet[Gender.Value].displayOf(pat.gender).get,
        pat.birthDate.toRight(NotAvailable),
        pat.managingZPM.toRight(NotAvailable),
        pat.insurance.toRight(NotAvailable),
        pat.dateOfDeath.toRight(NotAvailable)
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
        .map(c => ICDO3TDisplay(s"${c.code.value}: ${c.display.get}"))
        .getOrElse(ICDO3TDisplay(s"${icdO3T.code.value}: ${icdO3T.display.getOrElse("N/A")}"))
  }


  implicit def icdO3MtoDisplay(
    implicit icdO3: ICDO3Catalogs
  ): Coding[ICDO3M] => ICDO3MDisplay = {
    icdO3M =>
      icdO3.morphologyCodings()
        .find(_.code == icd.ICDO3.MorphologyCode(icdO3M.code.value))
        .map(c => ICDO3MDisplay(s"${c.code.value}: ${c.display.get}"))
        .getOrElse(ICDO3MDisplay(s"${icdO3M.code.value}: ${icdO3M.display.getOrElse("N/A")}"))
  }


  implicit def medicationToDisplay(
    implicit medications: MedicationCatalog
  ): Coding[Medication] => MedicationDisplay = {
    c =>
      medications
        .findByCode(med.Medication.Code(c.code.value))
        .map(c => MedicationDisplay(c.name.get))
        .getOrElse(MedicationDisplay(c.display.getOrElse("N/A")))
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


  implicit def guidelineTherapyToView[T <: GuidelineTherapy]: ((T,Option[Response])) => GuidelineTherapyView = {

    case (therapy,response) =>

      therapy match { 
      
        case th: PreviousGuidelineTherapy =>
          GuidelineTherapyView(
            th.id,
            th.diagnosis,
            th.therapyLine.toRight(NotAvailable),
            NotAvailable.asLeft[PeriodDisplay[LocalDate]],
            th.medication.map(_.mapTo[MedicationDisplay]),
            NotAvailable.asLeft[String],
            response.map(_.mapTo[ResponseDisplay]).toRight(NotAvailable)
          )
        
        case th: LastGuidelineTherapy =>
          GuidelineTherapyView(
            th.id,
            th.diagnosis,
            th.therapyLine.toRight(NotAvailable),
            th.period.map(_.mapTo[PeriodDisplay[LocalDate]]).toRight(NotAvailable),
            th.medication.map(_.mapTo[MedicationDisplay]),
            th.reasonStopped
              .flatMap(c => ValueSet[GuidelineTherapy.StopReason.Value].displayOf(c.code))
              .toRight(NotAvailable),
            response.map(_.mapTo[ResponseDisplay]).toRight(NotAvailable)
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

  implicit val ecogsToDisplay: List[ECOGStatus] => ECOGStatusView = {
    ecogs =>
      ECOGStatusView(
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
        finding.specimen,
        finding.performingInstitute.toRight(NotAvailable),
        finding.issuedOn.toRight(NotAvailable),
        finding.note
      )
  }



  implicit val tumorCellContentToDisplay: TumorCellContent => TumorCellContentDisplay = {
    tc =>
      val valuePercent = tc.value * 100
      val method       = ValueSet[TumorCellContent.Method.Value].displayOf(tc.method).get

      TumorCellContentDisplay(s"$valuePercent % ($method)")
  }


  implicit val histologyReportToView: HistologyReport => HistologyReportView = {
    report =>
      HistologyReportView(
        report.id,
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
  }


  import Variant.{StartEnd, Gene}

  implicit val startEndToDisplay: StartEnd => StartEndDisplay = {
    se =>
      se.end.fold(
        StartEndDisplay(se.start.toString)
      )(
        end => StartEndDisplay(s"${se.start} - $end")
      )   
  }


  implicit def geneCodingToDisplay(
    implicit hgnc: HGNCCatalog
  ): Coding[Gene] => GeneDisplay = {
    c =>
      hgncCatalog
        .geneWithSymbol(HGNCGene.Symbol(c.code.value))
        .map(g => s"${g.symbol.value}: ${g.name.get}")
        .map(GeneDisplay(_))
        .getOrElse(GeneDisplay("N/A"))
  }


  implicit def genesToDisplay: List[Gene] => Option[GeneDisplay] = {
    genes =>
      genes.map(_.value)
        .reduceLeftOption(_ + ", " + _).map(GeneDisplay(_))
  }


  implicit val simpleVariantToView: SimpleVariant => SimpleVariantView = {
    sv =>
      SimpleVariantView(
        sv.chromosome,
        sv.gene.mapTo[GeneDisplay],
        sv.startEnd.mapTo[StartEndDisplay],
        sv.refAllele,
        sv.altAllele,
        sv.functionalAnnotation.code,
        sv.dnaChange.code,
        sv.aminoAcidChange.code,
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
          s"SNV ${snv.gene.code.value} ${snv.aminoAcidChange.code.value}"
      
        case cnv: CNV => {
          val genes =
            cnv.reportedAffectedGenes
               .flatMap(_.map(_.code.value).reduceOption(_ + ", " + _))
               .getOrElse("N/A")
      
          s"CNV [${genes}], ${cnv.`type`}"
        }
      
        case dnaFusion: DNAFusion =>
          s"DNA-Fusion ${dnaFusion.mapTo[DNAFusionView].representation}"
      
        case rnaFusion: RNAFusion =>
          s"RNA-Fusion ${rnaFusion.mapTo[RNAFusionView].representation}"
      
        case rnaSeq: RNASeq =>
          s"RNA-Seq ${rnaSeq.gene.code.value}"
      
      }

      SupportingVariantDisplay(repr)
  }



  implicit val recommendationToDisplay:
    ((
     (TherapyRecommendation,ICD10Display),
     List[Variant]
    )) => TherapyRecommendationView = {


    case ((rec,icd10),variants) =>

      val supportingVariants = rec.supportingVariants.getOrElse(List.empty[Variant.Id])

      TherapyRecommendationView(
        rec.id,
        icd10,
        rec.medication.map(_.mapTo[MedicationDisplay]),
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

      val icd10 = diagnosis.icd10.map(_.mapTo[ICD10Display]).get

      CarePlanView(
        carePlan.id,
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
              diagnoses.find(_.id == cp.diagnosis).get,
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



  implicit val molecularTherapyToView: ((MolecularTherapy,Option[Response])) => MolecularTherapyView = {

    case (molTh,resp) =>

    val status   = ValueSet[MolecularTherapy.Status.Value].displayOf(molTh.status).get
    val note     = molTh.note.getOrElse("-")
    val response = resp.map(_.mapTo[ResponseDisplay]).toRight(NotAvailable)

    molTh match { 
 
      case th: NotDoneTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          NotAvailable.asLeft[PeriodDisplay[LocalDate]],
          ValueSet[MolecularTherapy.NotDoneReason.Value].displayOf(th.notDoneReason.code).toRight(NotAvailable),
          List.empty[MedicationDisplay],
          Undefined.asLeft[String],
          NotAvailable.asLeft[Dosage.Value],
          note,
          response
        )
       
      case th: StoppedTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          ValueSet[MolecularTherapy.StopReason.Value].displayOf(th.reasonStopped.code).toRight(NotAvailable),
          th.dosage.toRight(NotAvailable),
          note,
          response
        )
        
      case th: CompletedTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          Undefined.asLeft[String],
          th.dosage.toRight(NotAvailable),
          note,
          response
        )
        
      case th: OngoingTherapy => 
        MolecularTherapyView(
          th.id,
          status,
          th.recordedOn,
          th.basedOn,
          th.period.mapTo[PeriodDisplay[LocalDate]].asRight[NotAvailable],
          Undefined.asLeft[String],
          th.medication.toList.map(_.mapTo[MedicationDisplay]),
          Undefined.asLeft[String],
          th.dosage.toRight(NotAvailable),
          note,
          response
        )

    }

  }


  implicit val molecularTherapiesToView: ((List[MolecularTherapy],List[Response])) => List[MolecularTherapyView] = {
    case (therapies,responses) =>
      therapies.map(
        th => (th,responses.find(_.therapy == th.id)).mapTo[MolecularTherapyView]
      )
  }




  implicit val mtbFileToView: MTBFile => MTBFileView = {

    mtbfile =>

      val responses = mtbfile.responses.getOrElse(List.empty[Response])

      val ngsReports = mtbfile.ngsReports.getOrElse(List.empty[SomaticNGSReport])


      MTBFileView(
        mtbfile.patient.mapTo[PatientView],

        mtbfile.diagnoses.getOrElse(List.empty[Diagnosis])
          .map(_.mapTo[DiagnosisView]),

        mtbfile.familyMemberDiagnoses.getOrElse(List.empty[FamilyMemberDiagnosis])
          .map(_.mapTo[FamilyMemberDiagnosisView]),

        mtbfile.previousGuidelineTherapies.getOrElse(List.empty[PreviousGuidelineTherapy])
          .map(th => (th,responses.find(_.therapy == th.id)).mapTo[GuidelineTherapyView]) ++
            mtbfile.lastGuidelineTherapy
              .map(th => (th,responses.find(_.therapy == th.id)).mapTo[GuidelineTherapyView]),

        mtbfile.ecogStatus
          .map(_.mapTo[ECOGStatusView]),

        mtbfile.specimens.getOrElse(List.empty[Specimen])
          .map(_.mapTo[SpecimenView]),

        mtbfile.molecularPathologyFindings.getOrElse(List.empty[MolecularPathologyFinding])
          .map(_.mapTo[MolecularPathologyFindingView]),

        mtbfile.histologyReports.getOrElse(List.empty[HistologyReport])
          .map(_.mapTo[HistologyReportView]),

        ngsReports.map(_.mapTo[NGSReportView]),

        (
          (mtbfile.carePlans.getOrElse(List.empty[CarePlan]),
          mtbfile.diagnoses.getOrElse(List.empty[Diagnosis]),
          mtbfile.recommendations.getOrElse(List.empty[TherapyRecommendation]),
          mtbfile.studyInclusionRequests.getOrElse(List.empty[StudyInclusionRequest]),
          mtbfile.geneticCounsellingRequests.getOrElse(List.empty[GeneticCounsellingRequest])),
          ngsReports
        )
        .mapTo[List[CarePlanView]],

        (
          mtbfile.claims.getOrElse(List.empty[Claim]),
          mtbfile.claimResponses.getOrElse(List.empty[ClaimResponse])
        )
        .mapTo[List[ClaimStatusView]],

        (
          mtbfile.molecularTherapies.getOrElse(List.empty[MolecularTherapyDocumentation])
            .filterNot(_.history.isEmpty).map(_.history.head),
          responses 
        ).mapTo[List[MolecularTherapyView]]
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
