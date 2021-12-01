package de.bwhc.mtb.data


import java.net.URI
import java.time.{LocalDate,YearMonth}

import cats.data.NonEmptyList

import de.ekut.tbi.generators.{
  Gen, DateTimeGens
}


import de.bwhc.mtb.data.entry.dtos._

import de.bwhc.util.num._


package object gens
{

  //---------------------------------------------------------------------------
  // Patients
  //---------------------------------------------------------------------------

  implicit val genPatId: Gen[Patient.Id] =
    Gen.uuidStrings.map(Patient.Id)

  implicit val genZPM: Gen[ZPM] =
    Gen.oneOf("Freiburg","Heidelberg","Tübingen","Ulm")
       .map(ZPM(_))

  implicit val genIK: Gen[HealthInsurance.Id] =
    Gen.oneOf("AOK","Barmer","TK")
       .map(HealthInsurance.Id)

  implicit val genGender: Gen[Gender.Value] =
    Gen.distribution(
      (49.5,Gender.Male),
      (49.5,Gender.Female),
      (1,Gender.Other)
    )

  implicit val genPatient: Gen[Patient] =
    for {
      id      <- Gen.of[Patient.Id]
      g       <- Gen.of[Gender.Value]
      bd      <- DateTimeGens.localDatesBetween(
                   LocalDate.of(1950,1,1),
                   LocalDate.of(1980,12,31)
                 ) 
                 .map(d => YearMonth.of(d.getYear,d.getMonth))
      diedAge <- Gen.intsBetween(55,75)
      dod     =  Option(bd.plusYears(diedAge)).filterNot(_.isAfter(YearMonth.now))
      ik      <- Gen.of[HealthInsurance.Id]
    } yield Patient(id,g,Some(bd),None,Some(ik),dod)


  //---------------------------------------------------------------------------
  // Consent
  //---------------------------------------------------------------------------

  implicit val genConsentId: Gen[Consent.Id] =
    Gen.uuidStrings.map(Consent.Id)

  implicit val genConsentStatus: Gen[Consent.Status.Value] =
    Gen.enum(Consent.Status)

  def genConsentFor(pat: Patient): Gen[Consent] =
    for {
      id      <- Gen.of[Consent.Id]
      status  <- Gen.const(Consent.Status.Active)    
    } yield Consent(id,pat.id,status)


  //---------------------------------------------------------------------------
  // MTBEpisode
  //---------------------------------------------------------------------------

  implicit val genEpisodeId: Gen[MTBEpisode.Id] =
    Gen.uuidStrings.map(MTBEpisode.Id)

  def genEpisodeFor(pat: Patient): Gen[MTBEpisode] = 
    for {
      id      <- Gen.of[MTBEpisode.Id]

      dateOrder = Ordering[LocalDate]

      start   <- DateTimeGens.localDatesBetween(
                   dateOrder.min(pat.birthDate.get.plusYears(50).atEndOfMonth,LocalDate.now),
                   dateOrder.min(pat.birthDate.get.plusYears(55).atEndOfMonth,LocalDate.now),
                 )
    } yield MTBEpisode(id,pat.id,OpenEndPeriod(start))



  //---------------------------------------------------------------------------
  // Codings
  //---------------------------------------------------------------------------

  import ICD._

  implicit val genICD10GM: Gen[Coding[ICD10GM]] = 
    Gen.oneOf(icd10gmCodings)

  implicit val genICDO3T: Gen[Coding[ICDO3T]] = 
    Gen.oneOf(icdO3TCodings)

  implicit val genICDO3M: Gen[Coding[ICDO3M]] = 
    Gen.oneOf(icdO3MCodings)

  implicit val genWHOGrade: Gen[Coding[WHOGrade.Value]] =
    Gen.enum(WHOGrade).map(Coding(_,None,None))

  implicit val genMedication: Gen[Medication.Coding] = 
    Gen.oneOf(Medications.entries.filter(_.code.value.startsWith("L")))




  //---------------------------------------------------------------------------
  // Specimen
  //---------------------------------------------------------------------------

  implicit val genSpecimenId: Gen[Specimen.Id] =
    Gen.uuidStrings.map(Specimen.Id)

  implicit val genSpecimenType: Gen[Specimen.Type.Value] =
    Gen.enum(Specimen.Type)

  implicit val genSpecimenCollLoc: Gen[Specimen.Collection.Localization.Value] =
    Gen.enum(Specimen.Collection.Localization)

  implicit val genSpecimenCollMethod: Gen[Specimen.Collection.Method.Value] =
    Gen.enum(Specimen.Collection.Method)

  implicit val genCollection: Gen[Specimen.Collection] =
    for {
      loc    <- Gen.of[Specimen.Collection.Localization.Value]
      method <- Gen.of[Specimen.Collection.Method.Value]
    } yield Specimen.Collection(LocalDate.now,loc,method)

  def genSpecimenFor(pat: Patient): Gen[Specimen] = 
    for {
      id    <- Gen.of[Specimen.Id]
      icd10 <- Gen.of[Coding[ICD10GM]]
      typ   <- Gen.of[Specimen.Type.Value]
      coll  <- Gen.of[Specimen.Collection]      
    } yield Specimen(id,pat.id,icd10,Some(typ),Some(coll))



  //---------------------------------------------------------------------------
  // TumorCellContent
  //---------------------------------------------------------------------------

  def genTumorCellContentFor(
    specimen: Specimen,
    method: TumorCellContent.Method.Value 
  ): Gen[TumorCellContent] = 
    for {
      id <- Gen.uuidStrings.map(TumorCellContent.Id)
      tc <- Gen.doubles.map(_.withDecimals(5))
    } yield TumorCellContent(id,specimen.id,method,tc)
 

  //---------------------------------------------------------------------------
  // HistologyReport
  //---------------------------------------------------------------------------

  def genTumorMorphologyFor(
    specimen: Specimen
  ): Gen[TumorMorphology] =
    for {
      id     <- Gen.uuidStrings.map(TumorMorphology.Id)
      icdO3M <- Gen.of[Coding[ICDO3M]]
      note   =  Some("Histology finding notes...")
    } yield TumorMorphology(id,specimen.patient,specimen.id,icdO3M,note)

  def genHistologyReportFor(
    specimen: Specimen
  ): Gen[HistologyReport] =
    for {
      id     <- Gen.uuidStrings.map(HistologyReport.Id)
      morph  <- genTumorMorphologyFor(specimen)
      tc     <- genTumorCellContentFor(specimen,TumorCellContent.Method.Histologic)
    } yield HistologyReport(id,specimen.patient,specimen.id,Some(LocalDate.now),Some(morph),Some(tc))


  //---------------------------------------------------------------------------
  // MolecularPathologyFinding
  //---------------------------------------------------------------------------

  def genMolecularPathologyFindingFor(
    specimen: Specimen
  ): Gen[MolecularPathologyFinding] =
    for {
      id   <- Gen.uuidStrings.map(MolecularPathologyFinding.Id)
      dept <- Gen.uuidStrings.map(PathologyDept(_))
      note = "MolecularPathologyFinding notes..."
    } yield MolecularPathologyFinding(id,specimen.patient,specimen.id,Some(dept),Some(LocalDate.now),note)


  //---------------------------------------------------------------------------
  // NGS
  //---------------------------------------------------------------------------

  implicit val genSomaticNGSReportId: Gen[SomaticNGSReport.Id] =
    Gen.uuidStrings.map(SomaticNGSReport.Id)

  import SomaticNGSReport._

  implicit val genMSI: Gen[MSI] =
    Gen.doublesBetween(0,2)
      .map(_.withDecimals(2))
      .map(MSI)

  implicit val genTMB: Gen[TMB] =
    Gen.doublesBetween(0,1000000)
      .map(_.withDecimals(2))
      .map(TMB)

  implicit val genBRCAness: Gen[BRCAness] =
    Gen.doubles
      .map(_.withDecimals(2))
      .map(BRCAness)

  implicit val genAverageReadDepth: Gen[AverageReadDepth] =
    Gen.doublesBetween(0,40)
      .map(_.withDecimals(2))
      .map(AverageReadDepth)

  import Variant._
  import SimpleVariant._

/*
  implicit val genGeneSymbolCoding: Gen[Coding[GeneSymbol]] =
    Gen.oneOf(Genes.entries.unzip._2)

  implicit val genGeneId: Gen[Coding[HgncId]] =
    Gen.oneOf(Genes.entries.unzip._1)

//  val geneIdsCodings =
  implicit val geneIdsCoding: Gen[(Coding[HgncId],Coding[GeneSymbol])] =
    Gen.oneOf(Genes.entries)
*/
   
  implicit val genGeneCoding: Gen[Gene.Coding] =
    Gen.oneOf(Genes.entries)

  implicit val genCosmicId: Gen[CosmicId] =
    Gen.uuidStrings.map(CosmicId(_))

  implicit val genDbSNPId: Gen[DbSNPId] =
    Gen.uuidStrings.map(DbSNPId(_))

  private val alleles = List("A","C","G","T").map(Allele)

  implicit val genAllelicFreq: Gen[AllelicFrequency] =
    Gen.doubles
      .map(_.withDecimals(2))
      .map(AllelicFrequency)

  implicit val genAllelicReadDepth: Gen[AllelicReadDepth] =
    Gen.intsBetween(3,40).map(AllelicReadDepth)

  implicit val genStartEnd: Gen[StartEnd] =
    for {
      start <- Gen.positiveLongs 
      n     <- Gen.longsBetween(100,1000)
      end   =  start + n 
    } yield StartEnd(start,Some(end))


  implicit val genChromosome: Gen[Chromosome] =
    Gen.oneOf(Chromosome.instances)

  implicit val genInterpretation: Gen[Coding[Interpretation]] =
    Gen.oneOf(
      "Activating",
      "Inactivating",
      "Function Changed",
      "Probably Activating",
      "Probably Inactivating",
      "Probably Function Changed",
      "Ambiguous",
      "Benign"
    )
    .map(Interpretation(_))
    .map(Coding(_,None))


  implicit val genVariantId: Gen[Variant.Id] =
    Gen.uuidStrings.map(Variant.Id)

  implicit val genSimpleVariant: Gen[SimpleVariant] =
    for {
      chr       <- Gen.of[Chromosome]
      gene      <- Gen.of[Gene.Coding]
      se        <- Gen.positiveLongs.map(StartEnd(_,None))
      refAllele <- Gen.oneOf(alleles)
      altAllele <- Gen.oneOf(alleles.filterNot(_ == refAllele))
      dnaChg    =  Coding(DNAChange(s"${refAllele.value}>${altAllele.value}"), None)
      aaChg     =  Coding(AminoAcidChange(s"Amino acid change code..."), None)
      readDpth  <- Gen.of[AllelicReadDepth]
      allelicFreq <- Gen.of[AllelicFrequency]
      cosmicId  <- Gen.of[CosmicId]
      dbSNPId   <- Gen.of[DbSNPId]
      interpr   <- Gen.of[Coding[Interpretation]]
      id        <- Gen.uuidStrings.map(u => s"SNV_$u").map(Variant.Id)
    } yield SimpleVariant(
      id,chr,Some(gene),se,refAllele,altAllele,Some(dnaChg),Some(aaChg),
      readDpth,allelicFreq,Some(cosmicId),Some(dbSNPId),interpr
    )

  implicit val genCNV: Gen[CNV] =
    for {
      chr        <- Gen.of[Chromosome]
      startRange <- Gen.of[StartEnd]
      endRange   <- Gen.of[StartEnd]
      totalCN    <- Gen.intsBetween(2,5)
      relCN      <- Gen.doubles.map(_.withDecimals(2))
      cnA        <- Gen.doubles.map(_.withDecimals(2))
      cnB        <- Gen.doubles.map(_.withDecimals(2))
      genes      <- Gen.list(Gen.intsBetween(2,5),Gen.of[Gene.Coding])
      focality   =  "reported-focality..."
      typ        <- Gen.enum(CNV.Type)
      lohGenes      <- Gen.list(Gen.intsBetween(2,5),Gen.of[Gene.Coding])
      id         =  Variant.Id(
                      s"CNV_${genes.map(_.symbol.get.value).reduceLeft(_ + "_" + _)}_${typ.toString}"
                    ) 
    } yield CNV(
      id,chr,startRange,endRange,totalCN,relCN,Some(cnA),Some(cnB),
      Some(genes),Some(focality),typ,Some(lohGenes)
    )



  implicit val genDNAFusionPartner: Gen[DNAFusion.Partner] =
    for {
      chr  <- Gen.of[Chromosome]
      pos  <- Gen.positiveLongs
      gene <- Gen.of[Gene.Coding]
    } yield DNAFusion.Partner(chr,pos,gene)


  implicit val genDNAFusion: Gen[DNAFusion] =
    for {
      p5pr  <- Gen.of[DNAFusion.Partner]
      p3pr  <- Gen.of[DNAFusion.Partner]
      reads <- Gen.intsBetween(20,50)
      id    =  Variant.Id(s"DNAFusion_${p5pr.gene.symbol.get.value}_${p3pr.gene.symbol.get.value}") 
    } yield DNAFusion(id,Some(p5pr),Some(p3pr),Some(reads))


  implicit val genRNAFusionPartner: Gen[RNAFusion.Partner] = {
    import RNAFusion._

    for {
      gene       <- Gen.of[Gene.Coding]
      transcript <- Gen.uuidStrings.map(TranscriptId(_))
      exon       <- Gen.uuidStrings.map(ExonId)
      pos        <- Gen.positiveLongs.map(TranscriptPosition)
      strand     <- Gen.enum(Strand)
    } yield RNAFusion.Partner(gene,transcript,exon,pos,strand)

  }

  implicit val genRNAFusion: Gen[RNAFusion] =
    for {
      p5pr     <- Gen.of[RNAFusion.Partner]
      p3pr     <- Gen.of[RNAFusion.Partner]
      effect   <- Gen.const("RNA Fusion effect...").map(RNAFusion.Effect)
      cosmicId <- Gen.of[CosmicId]
      reads    <- Gen.intsBetween(20,50)
      id       =  Variant.Id(s"RNAFusion_${p5pr.gene.symbol.get.value}_${p3pr.gene.symbol.get.value}") 
    } yield RNAFusion(id,Some(p5pr),Some(p3pr),Some(effect),Some(cosmicId),Some(reads))



  implicit val genRNASeq: Gen[RNASeq] = 
    for {
      entrezId      <- Gen.uuidStrings.map(Gene.EntrezId)
      ensemblId     <- Gen.uuidStrings.map(Gene.EnsemblId)
      gene          <- Gen.of[Gene.Coding]
      transcript    <- Gen.uuidStrings.map(TranscriptId(_))
      fpkm          <- Gen.doubles.map(_.withDecimals(2))
      fromNGS       <- Gen.booleans
      tsCorrExp     <- Gen.booleans
      rawCounts     <- Gen.intsBetween(20,1000)
      librarySize   <- Gen.intsBetween(20,100) 
      cohortRanking <- Gen.intsBetween(1,10)
      id            =  Variant.Id(s"RNASeq_${entrezId.value}")
 
    } yield RNASeq(
      id,entrezId,ensemblId,gene,transcript,fpkm,fromNGS,tsCorrExp,
      rawCounts,librarySize,Some(cohortRanking)
    )

   implicit val genNGSReportMetadata: Gen[SomaticNGSReport.MetaData] = 
     for {
       kitType   <- Gen.const("Agilent ExomV6")
       kitManu   <- Gen.const("Agilent")
       sequencer <- Gen.const("Sequencer-XYZ")
       refGenome <- Gen.oneOf("HG19", "HG38", "GRCh37").map(ReferenceGenome(_))
       pipeline  <- Gen.const("dummy/uri/to/pipeline").map(URI.create)
     } yield SomaticNGSReport.MetaData(kitType,kitManu,sequencer,refGenome,Some(pipeline))


  def genSomaticNGSReportFor(
    specimen: Specimen
  ): Gen[SomaticNGSReport] =
    for {
      id             <- Gen.of[SomaticNGSReport.Id]
      patId          =  specimen.patient
      spId           =  specimen.id
      date           =  LocalDate.now
      seqType        <- Gen.oneOf("tNGS","WGS","WES").map(SomaticNGSReport.SequencingType(_))
      metadata       <- Gen.list(Gen.intsBetween(1,3), Gen.of[SomaticNGSReport.MetaData])
      tc             <- genTumorCellContentFor(specimen,TumorCellContent.Method.Bioinformatic)
      brcaness       <- Gen.of[BRCAness]
      msi            <- Gen.of[MSI]
      tmb            <- Gen.of[TMB]
      simpleVariants <- Gen.list(Gen.intsBetween(5,15), Gen.of[SimpleVariant])
      cnvs           <- Gen.list(Gen.intsBetween(5,10), Gen.of[CNV])
      dnaFusions     <- Gen.list(Gen.intsBetween(5,10), Gen.of[DNAFusion])
      rnaFusions     <- Gen.list(Gen.intsBetween(5,10), Gen.of[RNAFusion])
      rnaSeqs        <- Gen.list(Gen.intsBetween(5,10), Gen.of[RNASeq])

    } yield SomaticNGSReport(
      id,patId,spId,date,
      seqType,metadata,Some(tc),
      Some(brcaness),Some(msi),tmb,
      Some(simpleVariants),Some(cnvs),Some(dnaFusions),Some(rnaFusions),Some(rnaSeqs)
    )


  //---------------------------------------------------------------------------
  // Diagnosis
  //---------------------------------------------------------------------------

  implicit val genDiagnosisId: Gen[Diagnosis.Id] =
    Gen.uuidStrings.map(Diagnosis.Id)

  implicit val genDiagnosisStatus: Gen[Diagnosis.Status.Value] =
    Gen.enum(Diagnosis.Status)


  implicit val genDiagnosisStatusOnDate: Gen[Diagnosis.StatusOnDate] =
    for {
      status <- Gen.of[Diagnosis.Status.Value]
      date   = LocalDate.now
    } yield Diagnosis.StatusOnDate(status,date)


  def genDiagnosisFor(
    specimen: Specimen,
    histologyReports: List[HistologyReport]
  ): Gen[Diagnosis] =
    for {
      id        <- Gen.of[Diagnosis.Id]
      pat       =  specimen.patient
      date      =  LocalDate.now
      icd10     =  specimen.icd10
      icdO3T    =  icdO3TCodings.find(_.code.value == icd10.code.value)
      who       <- Gen.of[Coding[WHOGrade.Value]]
      histoRefs =  histologyReports.map(_.id) 
      status    <- Gen.listOf(2,Gen.of[Diagnosis.StatusOnDate])
      glTreatment <- Gen.enum(GuidelineTreatmentStatus)
    } yield Diagnosis(id,pat,Some(date),Some(icd10),icdO3T,Some(who),Some(histoRefs),Some(status),Some(glTreatment))


  //---------------------------------------------------------------------------
  // Family Member Diagnosis
  //---------------------------------------------------------------------------

  def genFamilyMemberDiagnosisFor(
    pat: Patient
  ): Gen[FamilyMemberDiagnosis] =
    for {
      id  <- Gen.uuidStrings.map(FamilyMemberDiagnosis.Id)
      rel <- Gen.enum(FamilyMember.Relationship).map(Coding(_,None))
    } yield FamilyMemberDiagnosis(id,pat.id,rel)


  //---------------------------------------------------------------------------
  // Guideline Therapies
  //---------------------------------------------------------------------------

  implicit val genTherapyId: Gen[TherapyId] =
    Gen.uuidStrings.map(TherapyId(_))

  implicit val genMedications: Gen[List[Medication.Coding]] =
    Gen.list(Gen.intsBetween(1,4), Gen.of[Medication.Coding])



  implicit val genGLTherapyStopReason: Gen[GuidelineTherapy.StopReason.Value] =
    Gen.enum(GuidelineTherapy.StopReason)


  def genPreviousGLTherapyFor(
    diag: Diagnosis
  ): Gen[PreviousGuidelineTherapy] = 
    for {
      id    <- Gen.of[TherapyId]
      thl   <- Gen.oneOf(TherapyLine.values)
      meds       <- Gen.of[List[Medication.Coding]]
    } yield PreviousGuidelineTherapy(id,diag.patient,diag.id,Some(thl),Some(meds))


  def genLastGLTherapyFor(
    diag: Diagnosis
  ): Gen[LastGuidelineTherapy] = 
    for {
      id         <- Gen.of[TherapyId]
      thl        <- Gen.oneOf(TherapyLine.values)
      start      =  diag.recordedOn.getOrElse(LocalDate.now)
      period     =  OpenEndPeriod(start,Some(start.plusWeeks(3)))
      meds       <- Gen.of[List[Medication.Coding]]
      stopReason <- Gen.of[GuidelineTherapy.StopReason.Value].map(Coding(_,None))
    } yield LastGuidelineTherapy(id,diag.patient,diag.id,Some(thl),Some(period),Some(meds),Some(stopReason))



  //---------------------------------------------------------------------------
  // ECOG Performance Status
  //---------------------------------------------------------------------------

  def genECOGStatusFor(
    pat: Patient
  ): Gen[ECOGStatus] =
    for {
      id    <- Gen.uuidStrings.map(ECOGStatus.Id)
      patId =  pat.id
      date  =  LocalDate.now
      ecog  <- Gen.enum(ECOG).map(Coding(_,None))
    } yield ECOGStatus(id,patId,Some(date),ecog)




  //---------------------------------------------------------------------------
  // CarePlan w/ contents
  //---------------------------------------------------------------------------


  implicit val genLOE: Gen[LevelOfEvidence] =
    for {
      grade <- Gen.enum(LevelOfEvidence.Grading).map(Coding(_,None))
      adds  <- Gen.subsets(LevelOfEvidence.Addendum.values.toSet)
    } yield LevelOfEvidence(grade,Some(adds.map(Coding(_,None))))


  def genTherapyRecommendationFor(
    diag: Diagnosis,
    ngsReport: SomaticNGSReport
  ): Gen[TherapyRecommendation] =
    for {
      id    <- Gen.uuidStrings.map(TherapyRecommendation.Id)
      date  =  LocalDate.now
      meds  <- Gen.of[List[Medication.Coding]]
      prio  <- Gen.enum(TherapyRecommendation.Priority)
      loe   <- Gen.of[LevelOfEvidence]
//      supportingVariantRefs <- Gen.subsets(ngsReport.variants).map(_.map(_.id))
      supportingVariantRefs <-
        Gen.subsets(
          ngsReport.simpleVariants.get ++ ngsReport.copyNumberVariants.get
        )
        .map(_.map(_.id))
    } yield TherapyRecommendation(
      id,diag.patient,diag.id,Some(date),Some(meds),Some(prio),Some(loe),Some(ngsReport.id),Some(supportingVariantRefs)
    )


  def genRebiopsyRequestFor(
    specimen: Specimen
  ): Gen[RebiopsyRequest] =
    for {
      id    <- Gen.uuidStrings.map(RebiopsyRequest.Id)
      patId =  specimen.patient
      spId  =  specimen.id
      date  =  LocalDate.now
    } yield RebiopsyRequest(id,patId,spId,Some(date))


  def genHistologyReevaluationRequestFor(
    specimen: Specimen
  ): Gen[HistologyReevaluationRequest] =
    for {
      id    <- Gen.uuidStrings.map(HistologyReevaluationRequest.Id)
      patId =  specimen.patient
      spId  =  specimen.id
      date  =  LocalDate.now
    } yield HistologyReevaluationRequest(id,patId,spId,Some(date))


  def genStudyInclusionRequestFor(
    diag: Diagnosis
  ): Gen[StudyInclusionRequest] =
    for {
      id     <- Gen.uuidStrings.map(StudyInclusionRequest.Id)
      nctNum <- Gen.intsBetween(10000000,99999999)
                   .map(num => NCTNumber(s"NCT$num"))
      date   =  LocalDate.now
    } yield StudyInclusionRequest(id,diag.patient,diag.id,nctNum,Some(date))


  def genCounsellingRequestFor(
    pat: Patient.Id
  ): Gen[GeneticCounsellingRequest] =
    for {
      id    <- Gen.uuidStrings.map(GeneticCounsellingRequest.Id)
      date  =  LocalDate.now
      reason = "Some reason for genetic counselling..."
    } yield GeneticCounsellingRequest(id,pat,Some(date),reason)


  def genCarePlanFor(
    diag: Diagnosis,
    ngsReport: SomaticNGSReport,
    specimens: List[Specimen]
  ): Gen[
      (CarePlan,NonEmptyList[TherapyRecommendation],
       Option[GeneticCounsellingRequest],List[RebiopsyRequest],Option[StudyInclusionRequest])
     ] =
    for {
      id          <- Gen.uuidStrings.map(CarePlan.Id)
      patId       =  diag.patient
      date        =  LocalDate.now
      descr       =  "MTB conference protocol..."
      recs        <- Gen.nonEmptyList(
                       Gen.intsBetween(2,4),
                       genTherapyRecommendationFor(diag,ngsReport)
                     )
      recRefs     =  recs.map(_.id).toList
      counsellingReq <- genCounsellingRequestFor(diag.patient)
      rebiopsyReqs    <- genRebiopsyRequestFor(specimens.head)
                          .map(List(_))
      rebiopsyReqRefs = rebiopsyReqs.map(_.id)
      studyInclusion <- genStudyInclusionRequestFor(diag)
    } yield (
      CarePlan(id,patId,diag.id,Some(date),Some(descr),None,Some(recRefs),Some(counsellingReq.id),Some(rebiopsyReqRefs),Some(List(studyInclusion.id))),
      recs,
      Some(counsellingReq), 
      rebiopsyReqs,
      Some(studyInclusion)
    )


  //---------------------------------------------------------------------------
  // Claim / ClaimResponse
  //---------------------------------------------------------------------------

  def genClaimWithResponseFor(
    rec: TherapyRecommendation
  ): Gen[(Claim,ClaimResponse)] =
    for {
      clId  <- Gen.uuidStrings.map(Claim.Id)
      patId =  rec.patient
      date  =  LocalDate.now
      thId  =  rec.id
      respId <- Gen.uuidStrings.map(ClaimResponse.Id)
      status <- Gen.enum(ClaimResponse.Status)
      reason <- Gen.enum(ClaimResponse.Reason)
    } yield (
      Claim(clId,patId,date,thId),
      ClaimResponse(respId,clId,patId,date,status,Some(reason))
    )


  //---------------------------------------------------------------------------
  // Response
  //---------------------------------------------------------------------------

  def genResponseFor(
    pat: Patient,
    th: TherapyId
  ): Gen[Response] =
    for {
      id     <- Gen.uuidStrings.map(Response.Id)
      patId  =  pat.id
      date   =  LocalDate.now
      recist <- Gen.enum(RECIST).map(Coding(_,None)) 
    } yield Response(id,patId,th,date,recist)


  //---------------------------------------------------------------------------
  // MolecularTherapy
  //---------------------------------------------------------------------------

  implicit val genDosage: Gen[Dosage.Value] =
    Gen.enum(Dosage)


  def genNotDoneTherapyFor(
    rec: TherapyRecommendation
  ): Gen[NotDoneTherapy] =
    for {
      id      <- Gen.of[TherapyId] 
      patId   =  rec.patient
      date    =  LocalDate.now
      basedOn =  rec.id
      reason  <- Gen.enum(MolecularTherapy.NotDoneReason).map(Coding(_,None))
      note    =  "Notes on the Therapy..."
    } yield NotDoneTherapy(id,patId,date,basedOn,reason,Some(note))


  def genStoppedTherapyFor(
    rec: TherapyRecommendation
  ): Gen[StoppedTherapy] =
    for {
      id      <- Gen.of[TherapyId] 
      patId   =  rec.patient
      date    =  LocalDate.now
      basedOn =  rec.id
      period  =  ClosedPeriod(LocalDate.now,LocalDate.now)
      meds    =  rec.medication
      dosage  <- Gen.of[Dosage.Value]
      reason  <- Gen.enum(MolecularTherapy.StopReason).map(Coding(_,None))
      note    =  "Notes on the Therapy..."
    } yield StoppedTherapy(id,patId,date,basedOn,period,meds,Some(dosage),reason,Some(note))


  def genOngoingTherapyFor(
    rec: TherapyRecommendation
  ): Gen[OngoingTherapy] =
    for {
      id      <- Gen.of[TherapyId] 
      patId   =  rec.patient
      date    =  LocalDate.now
      basedOn =  rec.id
      period  =  OpenEndPeriod(LocalDate.now)
      meds    =  rec.medication
      dosage  <- Gen.of[Dosage.Value]
      note    =  "Notes on the Therapy..."
    } yield OngoingTherapy(id,patId,date,basedOn,period,meds,Some(dosage),Some(note))


  def genCompletedTherapyFor(
    rec: TherapyRecommendation
  ): Gen[CompletedTherapy] =
    for {
      id      <- Gen.of[TherapyId] 
      patId   =  rec.patient
      date    =  LocalDate.now
      basedOn =  rec.id
      period  =  ClosedPeriod(LocalDate.now,LocalDate.now)
      meds    =  rec.medication
      dosage  <- Gen.of[Dosage.Value]
      note    =  "Notes on the Therapy..."
    } yield CompletedTherapy(id,patId,date,basedOn,period,meds,Some(dosage),Some(note))


/*
  def genMolecularTherapyDocFor(
    rec: TherapyRecommendation
  ): Gen[MolecularTherapyDocumentation] =
    for {
      notDone <- getNotDoneTherapyFor(rec)
      ongoing <- getOngoingTherapyFor(rec)
      stopped <- getStoppedTherapyFor(rec)
      compl   <- getCompletedTherapyFor(rec)
      seq     =  List(notDone,ongoing,stopped,compl)
    } yield MolecularTherapyDocumentation(seq)
*/

  def genMolecularTherapyDocFor(
    rec: TherapyRecommendation
  ): Gen[MolecularTherapyDocumentation] = {

    import MolecularTherapy.Status._
 
    for {
      status <- Gen.oneOf(NotDone,Ongoing,Stopped,Completed)
      th <-
        status match {
          case NotDone   => genNotDoneTherapyFor(rec)
          case Ongoing   => genOngoingTherapyFor(rec)
          case Stopped   => genStoppedTherapyFor(rec)
          case Completed => genCompletedTherapyFor(rec)
        }
      seq     =  List(th)
    } yield MolecularTherapyDocumentation(seq)

  }


  //---------------------------------------------------------------------------
  // MTBFile
  //---------------------------------------------------------------------------

  implicit val genMTBFile: Gen[MTBFile] =
    for {
      patient   <- Gen.of[Patient]

      consent   <- genConsentFor(patient)

      episode   <- genEpisodeFor(patient)

      specimen <- genSpecimenFor(patient)
      specimens = List(specimen)

      histology <- genHistologyReportFor(specimen).map(List(_)) 

      molPatho <- Gen.oneOfEach(
                     specimens.map(
                       genMolecularPathologyFindingFor(_)
                     )
                   )

      diagnosis <- genDiagnosisFor(specimen,histology)

      diagnoses = List(diagnosis)

      familyMemberDiagnoses <- Gen.listOf(2, genFamilyMemberDiagnosisFor(patient))

      ngsReport  <- genSomaticNGSReportFor(specimen)
      ngsReports =  List(ngsReport)

      previousGL <- Gen.listOf(3,genPreviousGLTherapyFor(diagnosis))

      lastGL     <- genLastGLTherapyFor(diagnosis).map(List(_))

      ecogs     <- Gen.list(
                     Gen.intsBetween(2,4),
                     genECOGStatusFor(patient)
                   )

      cpData    <- genCarePlanFor(diagnosis,ngsReport,specimens)

      (carePlan,
       recommendations,
       counsellingReq,
       rebiopsyReqs,
       studyInclusion) = cpData

      histoReeval <- Gen.oneOfEach(
                       specimens.map(
                         genHistologyReevaluationRequestFor(_)
                       )
                     )

      claimData <- Gen.oneOfEach(
                     recommendations.toList.map(genClaimWithResponseFor)
                   ) 

      (claims,claimResponses) = claimData.unzip    

      molThDocs <- Gen.oneOfEach(
                     recommendations.toList.map(genMolecularTherapyDocFor)
                   )

      responses <- Gen.oneOfEach(
                     molThDocs.map(_.history.head)
                       .filterNot(_.status == MolecularTherapy.Status.NotDone)
                       .map(th => genResponseFor(patient,th.id))
                   )

    } yield MTBFile(
      patient,
      consent,
      episode,
      Some(diagnoses),
      Some(familyMemberDiagnoses),
      Some(previousGL),
      Some(lastGL),
      Some(ecogs),
      Some(specimens),
      Some(molPatho),
      Some(histology),
      Some(ngsReports),
      Some(List(carePlan)),
      Some(recommendations.toList),
      counsellingReq.map(List(_)),
      Some(rebiopsyReqs),
      Some(histoReeval),
//      Some(studyInclusion),
      studyInclusion.map(List(_)),     
      Some(claims),
      Some(claimResponses),
      Some(molThDocs),
      Some(responses)
    )



}
