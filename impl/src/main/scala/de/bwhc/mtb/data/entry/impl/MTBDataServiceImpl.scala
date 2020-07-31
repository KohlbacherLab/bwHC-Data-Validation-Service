package de.bwhc.mtb.data.entry.impl


import java.time.Instant

import scala.util._

import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.NonEmptyList
import cats.instances.future._
import cats.syntax.apply._
import cats.syntax.either._

import de.bwhc.util.Logging

import de.bwhc.mtb.data.entry.api._

import de.bwhc.mtb.data.entry.dtos._



class MTBDataServiceProviderImpl extends MTBDataServiceProvider
{
    
  def getInstance: MTBDataService = {

    val localSite    = Option(System.getProperty("bwhc.zpm.site")).map(ZPM(_)).get  //TODO: improve configurability
    val validator    = DataValidator.getInstance.getOrElse(DefaultDataValidator)
    val db           = MTBDataDB.getInstance.get
    val queryService = QueryServiceProxy.getInstance.get
    
    new MTBDataServiceImpl(
      localSite,
      validator,
      db,
      queryService
    )
  }
    
}


object Helpers
{

  implicit class DataQualityReportOps(val qc: DataQualityReport) extends AnyVal
  {
    def hasErrors: Boolean = {
      qc.issues
        .map(_.severity)
        .toList
        .contains(DataQualityReport.Issue.Severity.Error)
    }
  }

}


class MTBDataServiceImpl
(
  private val localSite: ZPM,
  private val validator: DataValidator,
  private val db: MTBDataDB,
  private val queryService: QueryServiceProxy
)
extends MTBDataService
with Logging
{

  import Helpers._

  def process(
    cmd: MTBDataService.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,MTBDataService.Event]] = {

    import MTBDataService.Command._
    import MTBDataService.Event._

    cmd match {

      //-----------------------------------------------------------------------
      case Upload(data) => {

        log.info(s"Handling MTBFile upload for Patient ${data.patient.id.value}")

        //: Assign managingZPM to Patient
        val mtbfile = data.copy(patient = data.patient.copy(managingZPM = Some(localSite)))       

        validator.check(mtbfile)
          .flatMap {

            case Left(qcReport) =>
              (
                db.save(mtbfile),
                db.save(qcReport)
              )
              .mapN(
                (_,_) => qcReport.asLeft[MTBFile]
              )
              .andThen {
                case Success(Left(qc)) if (!qc.hasErrors) =>
                  queryService ! QueryServiceProxy.Command.Upload(mtbfile)
              }

            case Right(_) =>
              (queryService ! QueryServiceProxy.Command.Upload(mtbfile))
                .map(_ => mtbfile.asRight[DataQualityReport])
                .andThen {
                  case Success(_) => db.deleteAll(mtbfile.patient.id)
                }
 
          }
          .map(Imported(_).asRight[String])
          .recover {
            case t => t.getMessage.asLeft[MTBDataService.Event]
          }

      }

      //-----------------------------------------------------------------------
      case Delete(patId) => {

        log.info(s"Handling Delete request for data of $patId")

        (
          db.deleteAll(patId),
          queryService ! QueryServiceProxy.Command.Delete(patId)
        )
        .mapN(
          (_,_) => Deleted(patId).asRight[String]
        )
        .recover {
          case t => t.getMessage.asLeft[MTBDataService.Event]
        }
      }
    }

  }


  def patientsWithIncompleteData(
    implicit ec: ExecutionContext
  ): Future[Iterable[Patient.Id]] = {
    
    db.dataQcReports.map(_.map(_.patient))

  }


  def mtbfile(
    patient: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]] = {
    db.mtbfile(patient)
  }


  def dataQualityReport(
    patient: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[DataQualityReport]] = {

    db.dataQcReportOf(patient)

  }


}
