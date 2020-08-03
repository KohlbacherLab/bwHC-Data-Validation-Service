package de.bwhc.mtb.data


import java.time.LocalDate

import cats.data.NonEmptyList

import de.ekut.tbi.generators.{
  Gen, DateTimeGens
}

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
      dod     =  Option(bd.plusYears(diedAge)).filterNot(_.isBefore(LocalDate.now.minusDays(1)))
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
      rel <- Gen.enum(FamilyMember.Relationship)
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
      stopReason <- Gen.of[GuidelineTherapy.StopReason.Value]
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
      grade <- Gen.enum(LevelOfEvidence.Grading)
      adds  <- Gen.subsets(LevelOfEvidence.Addendum.values.toSet)
    } yield LevelOfEvidence(grade,Some(adds))


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
  ): Gen[(CarePlan,NonEmptyList[TherapyRecommendation],Option[GeneticCounsellingRequest],Option[List[RebiopsyRequest]])] =
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
      Some(rebiopyReqs)
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

//  implicit val genMolThStopReason: Gen[MolecularTherapy.StopReason.Value] =
//    Gen.enum(MolecularTherapy.StopReason)


/*
final case class NotDoneTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  basedOn: TherapyRecommendation.Id,
  notDoneReason: MolecularTherapy.NotDoneReason.Value,
  note: Option[String]
)

final case class OngoingTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  basedOn: TherapyRecommendation.Id,
  period: OpenEndPeriod[LocalDate],
  medication: NonEmptyList[Coding[Medication]],
  dosage: Option[Dosage.Value],
  note: Option[String]
)

final case class StoppedTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  basedOn: TherapyRecommendation.Id,
  period: ClosedPeriod[LocalDate],
  medication: NonEmptyList[Coding[Medication]],
  dosage: Option[Dosage.Value],
  reasonStopped: MolecularTherapy.StopReason.Value,
  note: Option[String]
)

final case class CompletedTherapy
(
  id: TherapyId,
  patient: Patient.Id,
  recordedOn: LocalDate,
  basedOn: TherapyRecommendation.Id,
  period: ClosedPeriod[LocalDate],
  medication: NonEmptyList[Coding[Medication]],
  dosage: Option[Dosage.Value],
  note: Option[String]
)
*/


  def getNotDoneTherapyFor(
    rec: TherapyRecommendation
  ): Gen[NotDoneTherapy] =
    for {
      id      <- Gen.of[TherapyId] 
      patId   =  rec.patient
      date    =  LocalDate.now
      basedOn =  rec.id
      reason  <- Gen.enum(MolecularTherapy.NotDoneReason)
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
      reason  <- Gen.enum(MolecularTherapy.StopReason)
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






}
