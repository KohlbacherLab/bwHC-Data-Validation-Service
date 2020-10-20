package de.bwhc.mtb.data.entry.dtos


import java.time.LocalDate

import play.api.libs.json.Json


final case class NGSSummary
(
//  id: SomaticNGSReport.Id,
  patient: Patient.Id,
  specimen: Specimen.Id,
  tumorEntity: Coding[ICD10GM],
  specimenType: Option[Specimen.Type.Value],
  sequencingType: SomaticNGSReport.SequencingType.Value,
  tumorCellContent: Double,
)


object NGSSummary
{

  implicit val format = Json.format[NGSSummary]


  implicit val fromSpecimenAndNGS: ((Specimen,SomaticNGSReport)) => NGSSummary = {

    case (specimen,ngs) =>

      NGSSummary(
        specimen.patient,
        specimen.id,
        specimen.icd10,
        specimen.`type`,
        ngs.sequencingType,
        ngs.tumorCellContent.value
      )

  }

}
