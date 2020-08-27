package de.bwhc.mtb.data.entry.dtos




object ValueSets
{

  import ValueSet.Concept


  implicit val gender: ValueSet[Gender.Value] =
    ValueSet(
      "Geschlecht",
      List(
        Concept(Gender.Male   ,"Männlich"),
        Concept(Gender.Female ,"Weiblich"),
        Concept(Gender.Other  ,"Sonstiges"),
        Concept(Gender.Unknown,"Unbekannt")
      )
    )



  implicit val diagnosisStatus: ValueSet[Diagnosis.Status.Value] =
    ValueSet(
      "DiagnoseStadium",
      List(
        Concept(Diagnosis.Status.TumorFree   , "Tumorfrei"),
        Concept(Diagnosis.Status.Local       , "Lokal"),
        Concept(Diagnosis.Status.Metastasized, "Metastasiert"),
        Concept(Diagnosis.Status.Unknown     , "Unbekannt")
      )
    )


  implicit val familyMemberRelationship: ValueSet[FamilyMember.Relationship.Value] =
    ValueSet(
      "Verwandtschaftsgrad",
      List(
        Concept(FamilyMember.Relationship.FamilyMember        , "Verwandter ersten Grades"),
        Concept(FamilyMember.Relationship.ExtendedFamilyMember, "Verwandter weiteren Grades")
      )
    )



  implicit val therapyLines =
    ValueSet[TherapyLine](
      "TherapieLinie",
      TherapyLine.values.map(tl => ValueSet.Concept(tl, s"Therapie Linie ${tl.value}"))
    )

  implicit val guidelineTherapyStopReasons: ValueSet[GuidelineTherapy.StopReason.Value] =
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




  implicit val specimenType: ValueSet[Specimen.Type.Value] =
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


  implicit val specimenLocalization: ValueSet[Specimen.Collection.Localization.Value] =
    ValueSet(
      "Proben-Lokalisierung",
      List(
        Concept(Specimen.Collection.Localization.PrimaryTumor, "Primärtumor"),
        Concept(Specimen.Collection.Localization.Metastasis,   "Metastase"),
        Concept(Specimen.Collection.Localization.Unknown,      "Unbekannt")
      )
    )


  implicit val specimenCollectionMethod: ValueSet[Specimen.Collection.Method.Value] =
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




  implicit val molecularTherapyNotDoneReason: ValueSet[MolecularTherapy.NotDoneReason.Value] =
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


  implicit val molecularTherapyStopReason: ValueSet[MolecularTherapy.StopReason.Value] =
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


  implicit val molecularTherapyStatusDE: ValueSet[MolecularTherapy.Status.Value] =
    ValueSet(
      "MolekularTherapie-Status",
      List(
        Concept(MolecularTherapy.Status.NotDone  ,"Nicht umgesetzt" ),
        Concept(MolecularTherapy.Status.Stopped  ,"Abgebrochen"  ),
        Concept(MolecularTherapy.Status.Ongoing  ,"Laufend" ),
        Concept(MolecularTherapy.Status.Completed,"Abgeschlossen")
      )
    )




  implicit val ecog: ValueSet[ECOG.Value] =
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


  implicit val recist: ValueSet[RECIST.Value] =
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


  implicit val whoGrade: ValueSet[WHOGrade.Value] =
    ValueSet(
      "WHOGradingOfCNSTumors",
      List(
        Concept(WHOGrade.I  ,"Pilocytic astrocytoma"),
        Concept(WHOGrade.II ,"Diffuse astrocytoma"),
        Concept(WHOGrade.III,"Anaplastic astrocytoma"),
        Concept(WHOGrade.IV ,"Glioblastoma"),
      )
    )





  implicit val claimResponseStatus: ValueSet[ClaimResponse.Status.Value] =
    ValueSet(
      "Kostenübernahme-Status",
      List(
        ValueSet.Concept(ClaimResponse.Status.Accepted,"Angenommen"),
        ValueSet.Concept(ClaimResponse.Status.Rejected,"Abgelehnt"),
      )
    )


  implicit val claimResponseReason: ValueSet[ClaimResponse.Reason.Value] =
    ValueSet(
      "Kostenübernahme-Ablehnungsgrund",
      List(
        ValueSet.Concept(ClaimResponse.Reason.InsufficientEvidence,       "Nicht ausreichende Evidenz"),
        ValueSet.Concept(ClaimResponse.Reason.StandardTherapyNotExhausted,"Standardtherapie nicht ausgeschöpft"),
        ValueSet.Concept(ClaimResponse.Reason.Other,                      "Weitere Gründe")
      )
    )






}
