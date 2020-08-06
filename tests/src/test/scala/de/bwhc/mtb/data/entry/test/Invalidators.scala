package de.bwhc.mtb.data.entry.test



import java.util.UUID.randomUUID

import de.bwhc.mtb.data.entry.dtos._


object Invalidators
{

  


  implicit class InvalidationOps[T](val t: T) extends AnyVal
  {
    def invalidate(implicit f: T => T) = f(t)
  }

  implicit val invalidMTBFile: MTBFile => MTBFile = {

    case mtbfile @ MTBFile(
      patient,
      consent,
      episode,
      diagnoses,
      _,
      previousGuidelineTherapies,
      lastGuidelineTherapy,
      ecogStatus,
      specimens,
      histologyResults,
      carePlans,
      recommendations,
      counsellingRequests,
      rebiopsyRequests,
      claims,
      claimResponses,
      _,
      responses
    ) =>

      mtbfile.copy(
        patient = patient.copy(birthDate = None)
//        diagnoses = diagnoses.map(_.map(_.copy(patient = Patient.Id(randomUUID.toString))))
      )

  }



}

