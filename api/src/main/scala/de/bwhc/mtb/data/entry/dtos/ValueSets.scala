package de.bwhc.mtb.data.entry.dtos




object ValueSets
{

  import ValueSet.Concept


  implicit val genderValueSet: ValueSet[Gender.Value] =
    ValueSet(
      "Geschlecht",
      List(
        Concept(Gender.Male   ,"Männlich"),
        Concept(Gender.Female ,"Weiblich"),
        Concept(Gender.Other  ,"Sonstiges"),
        Concept(Gender.Unknown,"Unbekannt")
      )
    )



  implicit val diagnosisStatusValueSet: ValueSet[Diagnosis.Status.Value] =
    ValueSet(
      "DiagnoseStadium",
      List(
        Concept(Diagnosis.Status.TumorFree   , "Tumorfrei"),
        Concept(Diagnosis.Status.Local       , "Lokal"),
        Concept(Diagnosis.Status.Metastasized, "Metastasiert"),
        Concept(Diagnosis.Status.Unknown     , "Unbekannt")
      )
    )



  implicit val guidelineTreatmentStatusValueSet: ValueSet[GuidelineTreatmentStatus.Value] =
    ValueSet(
      "Leitlinientherapie-Status",
      List(
        Concept(GuidelineTreatmentStatus.Exhaustive            , "Leitlinien ausgeschöpft"),
        Concept(GuidelineTreatmentStatus.NonExhaustive         , "Leitlinien nicht ausgeschöpft"),
        Concept(GuidelineTreatmentStatus.Impossible            , "Leitlinientherapie nicht möglich"),
        Concept(GuidelineTreatmentStatus.NoGuidelinesAvailable , "Keine Leitlinien vorhanden"),
        Concept(GuidelineTreatmentStatus.Unknown               , "Unbekannt"),
      )
    )



  implicit val familyMemberRelationshipValueSet: ValueSet[FamilyMember.Relationship.Value] =
    ValueSet(
      "Verwandtschaftsgrad",
      List(
        Concept(FamilyMember.Relationship.FamilyMember        , "Verwandter ersten Grades"),
        Concept(FamilyMember.Relationship.ExtendedFamilyMember, "Verwandter weiteren Grades")
      )
    )



  implicit val therapyLinesValueSet =
    ValueSet[TherapyLine](
      "TherapieLinie",
      TherapyLine.values.map(tl => ValueSet.Concept(tl, s"Therapie Linie ${tl.value}"))
    )

  implicit val guidelineTherapyStopReasonsValueSet: ValueSet[GuidelineTherapy.StopReason.Value] =
    ValueSet(
      "LeitlinienTherapie-Abbruchsgrund",
      List(
        Concept(GuidelineTherapy.StopReason.PatientWish,        "Auf Wunsch des Patienten"),
        Concept(GuidelineTherapy.StopReason.Progression,        "Progression"),
        Concept(GuidelineTherapy.StopReason.Toxicity,           "Toxizität"),
        Concept(GuidelineTherapy.StopReason.StateDeterioration, "Zustandsverschlechterung"),
        Concept(GuidelineTherapy.StopReason.Remission,          "Anhaltende Remission"),
        Concept(GuidelineTherapy.StopReason.Other,              "Weitere Gründe"),
        Concept(GuidelineTherapy.StopReason.Unknown,            "Unbekannt")
      )
    )


  implicit val recommendationPriorityValueSet: ValueSet[TherapyRecommendation.Priority.Value] =
    ValueSet(
      "Therapie-Empfehlung-Priorität",
      TherapyRecommendation.Priority
        .values
        .toList
        .map(p => ValueSet.Concept(p, p.toString))
    )


  implicit val specimenTypeValueSet: ValueSet[Specimen.Type.Value] =
    ValueSet(
      "Proben-Art",
      List(
        ValueSet.Concept(Specimen.Type.FreshTissue , "Frischgewebe" ),
        ValueSet.Concept(Specimen.Type.CryoFrozen  , "Cryo-frozen"  ),
        ValueSet.Concept(Specimen.Type.FFPE        , "FFPE"         ),
        ValueSet.Concept(Specimen.Type.LiquidBiopsy, "Liquid Biopsy"),
        ValueSet.Concept(Specimen.Type.Unknown     , "Unbekannt"    )
      )
    )


  implicit val specimenLocalizationValueSet: ValueSet[Specimen.Collection.Localization.Value] =
    ValueSet(
      "Proben-Lokalisierung",
      List(
        Concept(Specimen.Collection.Localization.PrimaryTumor, "Primärtumor"),
        Concept(Specimen.Collection.Localization.Metastasis,   "Metastase"),
        Concept(Specimen.Collection.Localization.Unknown,      "Unbekannt")
      )
    )


  implicit val specimenCollectionMethodValueSet: ValueSet[Specimen.Collection.Method.Value] =
    ValueSet(
      "Proben-Entnahmemethode",
      List(
        ValueSet.Concept(Specimen.Collection.Method.Biopsy      , "Biopsie"      ),
        ValueSet.Concept(Specimen.Collection.Method.Resection   , "Resektat"     ),
        ValueSet.Concept(Specimen.Collection.Method.LiquidBiopsy, "Liquid Biopsy"),
        ValueSet.Concept(Specimen.Collection.Method.Cytology    , "Zytologie"    ),
        ValueSet.Concept(Specimen.Collection.Method.Unknown     , "Unbekannt"    )
      )
    )


  implicit val tumorContentMethod: ValueSet[TumorCellContent.Method.Value] =
    ValueSet(
      "Tumorgehalt-Bestimmungsmethode",
      List(
        Concept(TumorCellContent.Method.Histologic,    "histologisch"),
        Concept(TumorCellContent.Method.Bioinformatic, "bioinformatisch"),
      )
    )


  implicit val cnvTypeValueSet: ValueSet[CNV.Type.Value] =
    ValueSet(
      "CNV-Typ",
      List(
        ValueSet.Concept(CNV.Type.Loss,          "Loss"),
        ValueSet.Concept(CNV.Type.LowLevelGain,  "Low-level Gain"),
        ValueSet.Concept(CNV.Type.HighLevelGain, "High-level Gain"),
      )
    )



  implicit val molecularTherapyNotDoneReasonValueSet: ValueSet[MolecularTherapy.NotDoneReason.Value] =
    ValueSet(
      "MolekularTherapie-Nichtumsetzungsgrund",
      List(
        Concept(MolecularTherapy.NotDoneReason.PaymentRefused,      "Kostenübernahme abgelehnt"),
        Concept(MolecularTherapy.NotDoneReason.PaymentPending,      "Kostenübernahme noch ausstehend"),
        Concept(MolecularTherapy.NotDoneReason.NoIndication,        "Klinisch keine Indikation"),
        Concept(MolecularTherapy.NotDoneReason.MedicalReason,       "Medizinische Gründe"),
        Concept(MolecularTherapy.NotDoneReason.PatientRefusal,      "Therapie durch Patient abgelehnt"),
        Concept(MolecularTherapy.NotDoneReason.PatientDeath,        "Tod"),
        Concept(MolecularTherapy.NotDoneReason.OtherTherapyChosen,  "Wahl einer anderen Therapie durch Behandler"),
        Concept(MolecularTherapy.NotDoneReason.ContinuedExternally, "Weiterbehandlung extern"),
        Concept(MolecularTherapy.NotDoneReason.LostToFU,            "Lost to follow-up"),
        Concept(MolecularTherapy.NotDoneReason.Other,               "Weitere Gründe"),
        Concept(MolecularTherapy.NotDoneReason.Unknown,             "Unbekannt")
      )
    )


  implicit val molecularTherapyStopReasonValueSet: ValueSet[MolecularTherapy.StopReason.Value] =
    ValueSet(
      "MolekularTherapie-Abbruchsgrund",
      List(
        Concept(MolecularTherapy.StopReason.Remission,           "Anhaltende Remission"),
        Concept(MolecularTherapy.StopReason.PatientWish,         "Auf Wunsch des Patienten"),
        Concept(MolecularTherapy.StopReason.PaymentEnded,        "Ende der Kostenübernahme"),
        Concept(MolecularTherapy.StopReason.MedicalReason,       "Medizinische Gründe"),
        Concept(MolecularTherapy.StopReason.Progression,         "Progression"),
        Concept(MolecularTherapy.StopReason.PatientDeath,        "Tod"),
        Concept(MolecularTherapy.StopReason.Toxicity,            "Toxizität"),
        Concept(MolecularTherapy.StopReason.OtherTherapyChosen,  "Wahl einer anderen Therapie durch Behandler"),
        Concept(MolecularTherapy.StopReason.ContinuedExternally, "Weiterbehandlung extern"),
        Concept(MolecularTherapy.StopReason.StateDeterioration,  "Zustandsverschlechterung"),
        Concept(MolecularTherapy.StopReason.Other,               "Weitere Gründe"),
        Concept(MolecularTherapy.StopReason.Unknown,             "Unbekannt")
      )
    )


  implicit val molecularTherapyStatusDEValueSet: ValueSet[MolecularTherapy.Status.Value] =
    ValueSet(
      "MolekularTherapie-Status",
      List(
        Concept(MolecularTherapy.Status.NotDone  ,"Nicht umgesetzt" ),
        Concept(MolecularTherapy.Status.Stopped  ,"Abgebrochen"  ),
        Concept(MolecularTherapy.Status.Ongoing  ,"Laufend" ),
        Concept(MolecularTherapy.Status.Completed,"Abgeschlossen")
      )
    )




  implicit val ecogValueSet: ValueSet[ECOG.Value] =
    ValueSet(
      "ECOGPerformanceStatus",
      List(
        Concept(ECOG.Zero , "ECOG 0"),
        Concept(ECOG.One  , "ECOG 1"),
        Concept(ECOG.Two  , "ECOG 2"),
        Concept(ECOG.Three, "ECOG 3"),
        Concept(ECOG.Four , "ECOG 4")
      )
    )

/*
  implicit val ecogValueSet: ValueSet[ECOG.Value] =
    ValueSet(
      "ECOGPerformanceStatus",
      List(
        Concept(ECOG.Zero , "ECOG 0: (Normale, uneingeschränkte Aktivität wie vor der Erkrankung)"),
        Concept(ECOG.One  , "ECOG 1: (Einschränkung bei körperlicher Anstrengung, aber gehfähig; leichte körperliche Arbeit bzw. Arbeit im Sitzen)"),
        Concept(ECOG.Two  , "ECOG 2: (Gehfähig, Selbstversorgung möglich, aber nicht arbeitsfähig; kann mehr als 50% der Wachzeit aufstehen)"),
        Concept(ECOG.Three, "ECOG 3: (Nur begrenzte Selbstversorgung möglich; ist 50% oder mehr der Wachzeit an Bett oder Stuhl gebunden)"),
        Concept(ECOG.Four , "ECOG 4: (Völlig pflegebedürftig, keinerlei Selbstversorgung möglich; völlig an Bett oder Stuhl gebunden)")
      )
    )

  implicit val ecogValueSet: ValueSet[ECOG.Value] =
    ValueSet(
      "ECOGPerformanceStatus",
      List(
	Concept(ECOG.Zero , "ECOG 0 (Normale, uneingeschränkte Aktivität wie vor der Erkrankung)",
	Concept(ECOG.One  , "ECOG 1: (Einschränkung bei körperlicher Anstrengung, aber gehfähig; leichte körperliche Arbeit bzw. Arbeit im Sitzen)",
	Concept(ECOG.Two  , "ECOG 2: (Gehfähig, Selbstversorgung möglich, aber nicht arbeitsfähig; kann mehr als 50% der Wachzeit aufstehen)",
	Concept(ECOG.Three, "ECOG 3: (Nur begrenzte Selbstversorgung möglich; ist 50% oder mehr der Wachzeit an Bett oder Stuhl gebunden)",
	Concept(ECOG.Four , "ECOG 4: (Völlig pflegebedürftig, keinerlei Selbstversorgung möglich; völlig an Bett oder Stuhl gebunden)",
	Concept(ECOG.Five , "ECOG 5: (Tod)"
    )
*/

  implicit val recistValueSet: ValueSet[RECIST.Value] =
    ValueSet(
      "RECIST",
      List(
        Concept(RECIST.CR, "Complete Response"),
        Concept(RECIST.PR, "Partial Response" ),
        Concept(RECIST.MR, "Mixed Response" ),
        Concept(RECIST.SD, "Stable Disease"   ),
        Concept(RECIST.PD, "Progressive Disease"),
        Concept(RECIST.NA, "Not assessable"),
        Concept(RECIST.NYA,"Not yet assessable")
      )
    )


  implicit val whoGradeValueSet: ValueSet[WHOGrade.Value] =
    ValueSet(
      "WHOGradingOfCNSTumors",
      List(
        Concept(WHOGrade.I  ,"Pilocytic astrocytoma"),
        Concept(WHOGrade.II ,"Diffuse astrocytoma"),
        Concept(WHOGrade.III,"Anaplastic astrocytoma"),
        Concept(WHOGrade.IV ,"Glioblastoma"),
      )
    )


  import LevelOfEvidence._

  implicit val levelOfEvidenceGradingValueSet: ValueSet[LevelOfEvidence.Grading.Value] =
    ValueSet(
      "LevelOfEvidence-Graduierung",
      List(
        Concept(Grading.m1A ,"In der gleichen Tumorentität wurde der prädiktive Wert des Biomarkers oder die klinische Wirksamkeit in einer Biomarker-stratifizierten Kohorte einer adäquat gepowerten prospektiven Studie oder Metaanalyse gezeigt."),
        Concept(Grading.m1B ,"In der gleichen Tumorentität wurde der prädiktive Wert des Biomarkers oder die klinische Wirksamkeit in einer retrospektiven Kohorte oder Fall-Kontroll-Studie gezeigt."),
        Concept(Grading.m1C ,"Ein oder mehrere Fallberichte in der gleichen Tumorentität."),
        Concept(Grading.m2A ,"In einer anderen Tumorentität wurde der prädiktive Wert des Biomarkers oder die klinische Wirksamkeit in einer Biomarker-stratifizierten Kohorte einer adäquat gepowerten prospektiven Studie oder Metaanalyse gezeigt."),
        Concept(Grading.m2B ,"In einer anderen Tumorentität wurde der prädiktive Wert des Biomarkers oder die Klinische Wirksamkeit in einer retrospektiven Kohorte oder Fall-Kontroll-Studie gezeigt."),
        Concept(Grading.m2C ,"Unabhängig von der Tumorentität wurde beim Vorliegen des Biomarkers eine klinische Wirksamkeit in einem oder mehreren Fallberichten gezeigt."),
        Concept(Grading.m3  ,"Präklinische Daten (in-vitro-/in-vivo-Modelle, funktionelle Untersuchungen) zeigen eine Assoziation des Biomarkers mit der Wirksamkeit der Medikation, welche durch eine wissenschaftliche Rationale gestützt wird."),
        Concept(Grading.m4  ,"Eine wissenschaftliche, biologische Rationale legt eine Assoziation des Biomarkers mit der Wirksamkeit der Medikation nahe, welche bisher nicht durch (prä)klinische Daten gestützt wird.")
      )
    )


  implicit val levelOfEvidenceAddendumValueSet: ValueSet[LevelOfEvidence.Addendum.Value] =
    ValueSet(
      "LevelOfEvidence-Zusatzverweise",
      List(
        Concept(Addendum.IS ,"in situ Daten aus Untersuchungen an Patientenmaterial (z.B. IHC, FISH) unterstützen den Evidenzgrad. Die unterstützende Methode kann in Klammern zusätzlich angegeben werden, z.B. Evidenzgrad 3 is (IHC)"),
        Concept(Addendum.IV ,"in vitro Daten/ in vivo-Modelle (z.B. PDX-Modelle) derselben Tumorentität unterstützen den Evidenzgrad. Die unterstützende Methode kann in Klammern angegeben werden, z.B. Evidenzgrad 2 iv (PDX)"),
        Concept(Addendum.Z ,"Zusatzverweis für Zulassungsstatus (Z= EMA-Zulassung liegt vor; Z(FDA)= nur FDA-Zulassung vorhanden)"),
        Concept(Addendum.R ,"Verweis, dass es sich hierbei um einen Resistenzmarker für eine bestimmte Therapie handelt"),
      )
    )





  implicit val claimResponseStatusValueSet: ValueSet[ClaimResponse.Status.Value] =
    ValueSet(
      "Kostenübernahme-Status",
      List(
        ValueSet.Concept(ClaimResponse.Status.Accepted,"Angenommen"),
        ValueSet.Concept(ClaimResponse.Status.Rejected,"Abgelehnt"),
      )
    )


  implicit val claimResponseReasonValueSet: ValueSet[ClaimResponse.Reason.Value] =
    ValueSet(
      "Kostenübernahme-Ablehnungsgrund",
      List(
        ValueSet.Concept(ClaimResponse.Reason.InsufficientEvidence,       "Nicht ausreichende Evidenz"),
        ValueSet.Concept(ClaimResponse.Reason.StandardTherapyNotExhausted,"Standardtherapie nicht ausgeschöpft"),
        ValueSet.Concept(ClaimResponse.Reason.Other,                      "Weitere Gründe")
      )
    )



  val allValueSets =
    (
      ValueSet[Gender.Value],
      ValueSet[Diagnosis.Status.Value],
      ValueSet[FamilyMember.Relationship.Value],
      ValueSet[TherapyLine],
      ValueSet[GuidelineTherapy.StopReason.Value],
      ValueSet[TherapyRecommendation.Priority.Value],
      ValueSet[Specimen.Type.Value],
      ValueSet[Specimen.Collection.Localization.Value],
      ValueSet[Specimen.Collection.Method.Value],
      ValueSet[CNV.Type.Value],
      ValueSet[MolecularTherapy.NotDoneReason.Value],
      ValueSet[MolecularTherapy.StopReason.Value],
      ValueSet[MolecularTherapy.Status.Value],
      ValueSet[ECOG.Value],
      ValueSet[RECIST.Value],
      ValueSet[WHOGrade.Value],
      ValueSet[LevelOfEvidence.Grading.Value],
      ValueSet[LevelOfEvidence.Addendum.Value],
      ValueSet[ClaimResponse.Status.Value],
      ValueSet[ClaimResponse.Reason.Value]
    )


}
