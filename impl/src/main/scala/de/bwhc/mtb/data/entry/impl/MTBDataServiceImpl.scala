package de.bwhc.mtb.data.entry.impl


import java.time.Instant

import scala.util._

import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.NonEmptyList
import cats.implicits._

import de.bwhc.util.Logging

import de.bwhc.mtb.data.entry.api._

import de.bwhc.mtb.data.entry.dtos._



class MTBDataServiceProviderImpl extends MTBDataServiceProvider
{
    
  def getInstance: MTBDataService = {

    val validator    = DataValidator.getInstance.getOrElse(DefaultDataValidator)
    val db           = MTBDataDB.getInstance.get
    val queryService = QueryService.getInstance.get
    
    new MTBDataServiceImpl(validator,db,queryService)
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
  private val validator: DataValidator,
  private val db: MTBDataDB,
  private val queryService: QueryService
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
      case Upload(mtbfile) => {

        log.info("Handling data upload")

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
                  queryService ! QueryService.Command.Upload(mtbfile)
              }

            case Right(_) =>
              (queryService ! QueryService.Command.Upload(mtbfile))
                .map(_ => mtbfile.asRight[DataQualityReport])
 
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
          db.delete(patId),
          queryService ! QueryService.Command.Delete(patId)
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

