package de.bwhc.mtb.data


import java.time.LocalDate

import cats.data.NonEmptyList

import de.ekut.tbi.generators.{
  Gen, DateTimeGens
}

//import de.bwhc.util.data.Interval

import de.bwhc.mtb.data.entry.dtos._


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
      diedAge <- Gen.intsBetween(55,75)
      dod     =  Option(bd.plusYears(diedAge)).filterNot(_.isAfter(LocalDate.now.minusDays(1)))
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
      status  <- Gen.of[Consent.Status.Value]    
    } yield Consent(id,pat.id,status)


  //---------------------------------------------------------------------------
  // MTBEpisode
  //---------------------------------------------------------------------------

  implicit val genEpisodeId: Gen[MTBEpisode.Id] =
    Gen.uuidStrings.map(MTBEpisode.Id)

  def genEpisodeFor(pat: Patient): Gen[MTBEpisode] = 
    for {
      id      <- Gen.of[MTBEpisode.Id]
      start   <- DateTimeGens.localDatesBetween(
                   pat.birthDate.get.plusYears(50),
                   pat.birthDate.get.plusYears(55)
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

  implicit val genMedication: Gen[Coding[Medication]] = 
    Gen.oneOf(Medications.entries)




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
  // HistologyResult
  //---------------------------------------------------------------------------

  implicit val genHistologyResultId: Gen[HistologyResult.Id] =
    Gen.uuidStrings.map(HistologyResult.Id)

  def genHistologyResultFor(
    specimen: Specimen
  ): Gen[HistologyResult] =
    for {
      id     <- Gen.of[HistologyResult.Id]
      icdO3M <- Gen.of[Coding[ICDO3M]]
      note   =  Some("Histology finding notes...")
    } yield HistologyResult(id,specimen.patient,specimen.id,Some(LocalDate.now),Some(icdO3M),note)


  //---------------------------------------------------------------------------
  // NGS
  //---------------------------------------------------------------------------

  def genTumorContentFor(
    specimen: Specimen
  ): Gen[List[TumorContent]] = 
    for {
      path   <- Gen.doubles
      bioinf <- Gen.doubles
    } yield List(
      TumorContent(specimen.id,TumorContent.Method.Pathologic,path),
      TumorContent(specimen.id,TumorContent.Method.Bioinformatic,bioinf)
    )
 
  implicit val genSomaticNGSReportId: Gen[SomaticNGSReport.Id] =
    Gen.uuidStrings.map(SomaticNGSReport.Id)

  import SomaticNGSReport._

  implicit val genMSI: Gen[MSI] =
    Gen.doublesBetween(0,2).map(MSI)

  implicit val genTMB: Gen[TMB] =
    Gen.doublesBetween(0,1000000).map(TMB)

  implicit val genBRCAness: Gen[BRCAness] =
    Gen.doubles.map(BRCAness)

  implicit val genAverageReadDepth: Gen[AverageReadDepth] =
    Gen.doublesBetween(0,40).map(AverageReadDepth)

  import Variant._
  import SimpleVariant._


  implicit val genGeneCoding: Gen[Coding[Gene]] =
    Gen.oneOf(Genes.entries)

  implicit val genCosmicId: Gen[CosmicId] =
    Gen.uuidStrings.map(CosmicId(_))

  implicit val genDbSNPId: Gen[Coding[DbSNPId]] =
    Gen.uuidStrings.map(DbSNPId(_)).map(Coding(_,None))

  private val alleles = List("A","C","G","T").map(Allele)

  implicit val genAllelicFreq: Gen[AllelicFrequency] =
    Gen.doubles.map(AllelicFrequency)

  implicit val genAllelicReadDepth: Gen[AllelicReadDepth] =
    Gen.intsBetween(3,40).map(AllelicReadDepth)

  implicit val genStartEnd: Gen[StartEnd] =
    for {
      start <- Gen.longs 
    } yield StartEnd(start,Some(start+1))

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
      "Benign",
      "NotAvailable"
    )
    .map(Interpretation(_))
    .map(Coding(_,None))

   implicit val genSimpleVariant: Gen[SimpleVariant] =
     for {
       chr       <- Gen.of[Chromosome]
       gene      <- Gen.of[Coding[Gene]]
       se        <- Gen.of[StartEnd]
       refAllele <- Gen.oneOf(alleles)
       altAllele <- Gen.oneOf(alleles.filterNot(_ == refAllele))
       dnaChg    =  Coding(DNAChange(s"${refAllele.value}>${altAllele.value}"), None)
       aaChg     =  Coding(AminoAcidChange(s"${refAllele.value}>${altAllele.value}"), None)
       readDpth  <- Gen.of[AllelicReadDepth]
       allelicFreq <- Gen.of[AllelicFrequency]
       cosmicId  <- Gen.of[CosmicId]
       dbSNPId   <- Gen.of[Coding[DbSNPId]]
       interpr   <- Gen.of[Coding[Interpretation]]
     } yield SimpleVariant(
       chr,gene,se,refAllele,altAllele,dnaChg,aaChg,readDpth,allelicFreq,cosmicId,dbSNPId,interpr
     )

  def genSomaticNGSReportFor(
    specimen: Specimen
  ): Gen[SomaticNGSReport] =
    for {
      id    <- Gen.of[SomaticNGSReport.Id]
      patId =  specimen.patient
      spId  =  specimen.id
      date  = LocalDate.now
      tc    <- genTumorContentFor(specimen)
      brcaness <- Gen.of[BRCAness]
      msi      <- Gen.of[MSI]
      tmb      <- Gen.of[TMB]
      variants <- Gen.list(
                    Gen.intsBetween(5,20),
                    Gen.of[SimpleVariant]
                  )
    } yield SomaticNGSReport(
      id,patId,spId,date,tc,brcaness,msi,tmb,variants
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
    histologyResults: List[HistologyResult]
  ): Gen[Diagnosis] =
    for {
      id        <- Gen.of[Diagnosis.Id]
      pat       =  specimen.patient
      date      =  LocalDate.now
      icd10     =  specimen.icd10
      icdO3T    =  icdO3TCodings.find(_.code.value == icd10.code.value)
      who       <- Gen.of[Coding[WHOGrade.Value]]
      histoRefs =  histologyResults.map(_.id) 
      status    <- Gen.listOf(2,Gen.of[Diagnosis.StatusOnDate])
    } yield Diagnosis(id,pat,Some(date),Some(icd10),icdO3T,Some(who),Some(histoRefs),Some(status))


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

  implicit val genMedications: Gen[List[Coding[Medication]]] =
    Gen.list(Gen.intsBetween(1,4), Gen.of[Coding[Medication]])



  implicit val genGLTherapyStopReason: Gen[GuidelineTherapy.StopReason.Value] =
    Gen.enum(GuidelineTherapy.StopReason)


  def genPreviousGLTherapyFor(
    pat: Patient
  ): Gen[PreviousGuidelineTherapy] = 
    for {
      id    <- Gen.of[TherapyId]
      patId =  pat.id
      thl   <- Gen.oneOf(TherapyLine.values)
      meds  <- Gen.of[List[Coding[Medication]]]
    } yield PreviousGuidelineTherapy(id,patId,Some(thl),Some(meds))


  def genLastGLTherapyFor(
    pat: Patient
  ): Gen[LastGuidelineTherapy] = 
    for {
      id         <- Gen.of[TherapyId]
      patId      =  pat.id
      thl        <- Gen.oneOf(TherapyLine.values)
      period     =  OpenEndPeriod(LocalDate.now)
      meds       <- Gen.of[List[Coding[Medication]]]
      stopReason <- Gen.of[GuidelineTherapy.StopReason.Value].map(Coding(_,None))
    } yield LastGuidelineTherapy(id,patId,Some(thl),Some(period),Some(meds),Some(stopReason))



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
    pat: Patient
  ): Gen[TherapyRecommendation] =
    for {
      id    <- Gen.uuidStrings.map(TherapyRecommendation.Id)
      patId =  pat.id
      date  =  LocalDate.now
      meds  <- Gen.of[List[Coding[Medication]]]
                 .map(NonEmptyList.fromListUnsafe)
      prio  <- Gen.enum(TherapyRecommendation.Priority)
      loe   <- Gen.of[LevelOfEvidence]
      //TODO: ref supporting variant
    } yield TherapyRecommendation(id,patId,Some(date),meds,Some(prio),Some(loe),None)


  def genRebiopsyRequestFor(
    specimen: Specimen
  ): Gen[RebiopsyRequest] =
    for {
      id    <- Gen.uuidStrings.map(RebiopsyRequest.Id)
      patId =  specimen.patient
      spId  =  specimen.id
      date  =  LocalDate.now
    } yield RebiopsyRequest(id,patId,spId,Some(date))


  def genCounsellingRequestFor(
    pat: Patient
  ): Gen[GeneticCounsellingRequest] =
    for {
      id    <- Gen.uuidStrings.map(GeneticCounsellingRequest.Id)
      patId =  pat.id
      date  =  LocalDate.now
    } yield GeneticCounsellingRequest(id,patId,Some(date))


  def genCarePlanFor(
    pat: Patient,
    specimens: List[Specimen]
  ): Gen[(CarePlan,NonEmptyList[TherapyRecommendation],Option[GeneticCounsellingRequest],List[RebiopsyRequest])] =
    for {
      id          <- Gen.uuidStrings.map(CarePlan.Id)
      patId       =  pat.id
      date        =  LocalDate.now
      descr       =  "MTB conference protocol..."
      recs        <- Gen.nonEmptyList(
                       Gen.intsBetween(2,4),
                       genTherapyRecommendationFor(pat)
                     )
      recRefs     =  recs.map(_.id)
      counsellingReq <- genCounsellingRequestFor(pat)
      rebiopyReqs    <- genRebiopsyRequestFor(specimens.head)
                          .map(List(_))
      rebiopyReqRefs = rebiopyReqs.map(_.id)                 
    } yield (
      CarePlan(id,patId,Some(date),Some(descr),recRefs,Some(counsellingReq.id),Some(rebiopyReqRefs)),
      recs,
      Some(counsellingReq), 
      rebiopyReqs
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


  def getNotDoneTherapyFor(
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


  def getStoppedTherapyFor(
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


  def getOngoingTherapyFor(
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


  def getCompletedTherapyFor(
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


  //---------------------------------------------------------------------------
  // MTBFile
  //---------------------------------------------------------------------------

  implicit val genMTBFile: Gen[MTBFile] =
    for {
      patient   <- Gen.of[Patient]
      consent   <- genConsentFor(patient)
      episode   <- genEpisodeFor(patient)
      specimens <- Gen.list(
                     Gen.intsBetween(1,4),
                     genSpecimenFor(patient)
                   )
      histology <- Gen.oneOfEach(
                     specimens.map(
                       sp => Gen.list(
                         Gen.intsBetween(1,3),
                         genHistologyResultFor(sp)
                       )
                     )

                   ) 
      diagnoses <- Gen.oneOfEach(
                     (specimens zip histology).map { case (sp,hs) => genDiagnosisFor(sp,hs)}
                   )

      familyMemberDiagnoses <- Gen.listOf(2, genFamilyMemberDiagnosisFor(patient))


      ngsReports <- Gen.oneOfEach(
                      specimens.map(genSomaticNGSReportFor)
                    )

      previousGL <- Gen.listOf(3,genPreviousGLTherapyFor(patient))

      lastGL     <- genLastGLTherapyFor(patient)

      ecogs     <- Gen.list(
                     Gen.intsBetween(2,4),
                     genECOGStatusFor(patient)
                   )

      cpBatches  <- Gen.list(
                      Gen.intsBetween(1,3),
                      genCarePlanFor(patient,specimens)
                    )

      cpData    <- genCarePlanFor(patient,specimens)

      (carePlan,
       recommendations,
       counsellingReq,
       rebiopsyReqs) = cpData

      claimData <- Gen.oneOfEach(
                     recommendations.toList.map(genClaimWithResponseFor)
                   ) 

      (claims,claimResponses) = claimData.unzip    

      molThDocs <- Gen.oneOfEach(
                     recommendations.toList.map(genMolecularTherapyDocFor)
                   )

      responses <- Gen.oneOfEach(
                     molThDocs.map(_.history.head.id).map(genResponseFor(patient,_))
                   )

/*
final case class MTBFile
(
  patient: Patient,
  consent: Consent,
  episode: MTBEpisode,
  diagnoses: Option[List[Diagnosis]],
  familyMemberDiagnoses: Option[List[FamilyMemberDiagnosis]],
  previousGuidelineTherapies: Option[List[PreviousGuidelineTherapy]],
  lastGuidelineTherapy: Option[LastGuidelineTherapy],
  ecogStatus: Option[List[ECOGStatus]],
  specimens: Option[List[Specimen]],
  histologyResults: Option[List[HistologyResult]],
  //TODO: NGS reports
  carePlans: Option[List[CarePlan]],
  recommendations: Option[List[TherapyRecommendation]],
  geneticCounsellingRequests: Option[List[GeneticCounsellingRequest]],
  rebiopsyRequests: Option[List[RebiopsyRequest]],
  claims: Option[List[Claim]],
  claimResponses: Option[List[ClaimResponse]],
  molecularTherapies: Option[List[MolecularTherapyDocumentation]],
  responses: Option[List[Response]]
)
*/

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
      Some(histology.flatten),
      Some(ngsReports),
      Some(List(carePlan)),
      Some(recommendations.toList),
      counsellingReq.map(List(_)),
      Some(rebiopsyReqs),
      Some(claims),
      Some(claimResponses),
      Some(molThDocs),
      Some(responses)
    )



}
