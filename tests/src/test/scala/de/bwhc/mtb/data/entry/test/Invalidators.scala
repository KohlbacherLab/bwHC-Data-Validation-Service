package de.bwhc.mtb.data.entry.test



import java.util.UUID.randomUUID

import de.bwhc.mtb.data.entry.dtos._


object Invalidators
{


  implicit class InvalidationOps(val mtbfile: MTBFile) extends AnyVal
  {

    def withFatalIssues: MTBFile =
      mtbfile.copy(
        diagnoses = mtbfile.diagnoses.map(_.map(_.copy(icd10 = None))),
        specimens = None
      )


    def withErrors: MTBFile =
      mtbfile.copy(
        patient = mtbfile.patient.copy(birthDate = None),
      )


    def withWarnings: MTBFile =
      mtbfile.copy(
        claims = None,
        claimResponses = None,
        responses = None,
      )


  }


}

