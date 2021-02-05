package de.bwhc.mtb.data.entry.impl


import java.time.Instant

import scala.util._

import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.NonEmptyList
import cats.data.Validated._

import cats.Apply
import cats.instances.future._
import cats.syntax.apply._
import cats.syntax.either._

import de.bwhc.util.Logging

import de.bwhc.mtb.data.entry.api._
import DataQualityReport.Issue.Severity

import de.bwhc.mtb.data.entry.dtos._
import de.bwhc.mtb.data.entry.views.{MTBFileView,NotAvailable}
import de.bwhc.mtb.data.entry.views.mappings._




class MTBDataServiceProviderImpl extends MTBDataServiceProvider
{
    
  def getInstance: MTBDataService = {
    MTBDataServiceImpl.instance
  }
    
}

object MTBDataServiceImpl
{

  private val localSite    = Option(System.getProperty("bwhc.zpm.site")).map(ZPM(_)).get  //TODO: improve configurability
  private val db           = MTBDataDB.getInstance.get
  private val queryService = QueryServiceProxy.getInstance.get

  private val validator: DataValidator = new DefaultDataValidator

  val instance =
    new MTBDataServiceImpl(
      localSite,
      validator,
      db,
      queryService
    )

}


object Matchers
{

  import Severity._

  object FatalErrors
  {
    def unapply(rep: DataQualityReport): Boolean = {
      rep.issues.map(_.severity).toList contains Fatal
    }
  }

  object Errors
  {
    def unapply(rep: DataQualityReport): Boolean = {
      rep.issues.map(_.severity).toList contains Error
    }
  }

  object AtMostWarnings
  {
    def unapply(rep: DataQualityReport): Boolean = {
      rep.issues.map(_.severity).toList forall (s => s == Warning || s == Info)
    }
  }

  object OnlyInfos
  {
    def unapply(rep: DataQualityReport): Boolean = {
      rep.issues.map(_.severity).toList forall (_ == Info)
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

  import Matchers._

  import MTBDataService.Command._
  import MTBDataService.Response._
  import MTBDataService.Error._

  import ValueSets._

  implicit val patientDataInfo: ((Patient,DataQualityReport)) => PatientDataInfo = {
    case (patient,dataQC) =>
      PatientDataInfo(
        patient.id,
        ValueSet[Gender.Value].displayOf(patient.gender).get,
        patient.birthDate.toRight(NotAvailable),
        dataQC.issues.size
      )
  }


  override def process(
    cmd: MTBDataService.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[MTBDataService.Error,MTBDataService.Response]] = {

    cmd match {

      //-----------------------------------------------------------------------
      case Upload(data) => {

        log.info(s"Handling MTBFile upload for Patient ${data.patient.id.value}")

        //: Assign managingZPM to Patient
        val mtbfile = data.copy(patient = data.patient.copy(managingZPM = Some(localSite)))       

        val result =
          for {
            validation <- validator check mtbfile

            response <-
              validation match {

                case Invalid(qcReport) => {

                  qcReport match {

                    case FatalErrors() => {
                      log.error(s"FATAL issues detected, refusing data upload")
                      Future.successful(InvalidData(qcReport).asLeft[MTBDataService.Response])
                    }

                    case Errors() => {
                      log.warn(s"'ERROR'-level issues detected, storing DataQualityReport")
                      Apply[Future].*>(db save mtbfile)(db save qcReport)
                        .map(IssuesDetected(_).asRight[MTBDataService.Error])
                    }

                    case AtMostWarnings() => {
                      log.info(s"At most 'WARNING'-level issues detected. Forwarding data to QueryService, but storing DataQualityReport")
                      processAcceptable(mtbfile) andThen {
                        case Success(_) => {
                          db save mtbfile
                          db save qcReport
                        }
                      }
                    }

                    case OnlyInfos() => {
                      log.info(s"No issues detected, forwarding data to QueryService")
                      processAcceptable(mtbfile)
                    }

/*
                    case _ => {

                      log.warn(s"Issues detected, storing DataQualityReport")
                      Apply[Future].*>(db save mtbfile)(db save qcReport)
                        .map(IssuesDetected(_).asRight[MTBDataService.Error])
                    }
*/
                  }
                
                }

                case Valid(_) => {
                  log.info(s"No issues detected, forwarding data to QueryService")
                  processAcceptable(mtbfile)
                }
  
              }
  
          } yield response

        result.recover {
          case t => UnspecificError(t.getMessage).asLeft[MTBDataService.Response]
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
          (_,_) => Deleted(patId).asRight[MTBDataService.Error]
        )
        .recover {
          case t => UnspecificError(t.getMessage).asLeft[MTBDataService.Response]
        }
      }
    }

  }


  private def processAcceptable(
    mtbfile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[Either[MTBDataService.Error,MTBDataService.Response]] = {

    (queryService ! QueryServiceProxy.Command.Upload(mtbfile))
      .andThen {
        case Success(_) => db deleteAll mtbfile.patient.id
      }
      .map(_ => Imported(mtbfile).asRight[MTBDataService.Error])

  }


  override def patientsWithIncompleteData(
    implicit ec: ExecutionContext
  ): Future[Iterable[PatientDataInfo]] = {
  
    log.info(s"Handling request for Patients with data quality issues")

    for {
      mtbfiles <- db.mtbfiles
      infos    <- Future.sequence(
                    mtbfiles.map { mtbfile =>
                      val patient = mtbfile.patient
                      db.dataQcReportOf(patient.id)
                        .map(qc => (patient,qc.get).mapTo[PatientDataInfo])
                    }
                  )
    } yield infos 
    

  }


  override def mtbfile(
    patient: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]] = {

    log.info(s"Handling request for MTBFile of Patient ${patient.value}")

    db.mtbfile(patient)

  }


  override def mtbfileView(
    patient: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFileView]] = {

    for {
      mtbf <- mtbfile(patient)
      view =  mtbf.map(_.mapTo[MTBFileView])
    } yield view

  }


  override def dataQualityReport(
    patient: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[DataQualityReport]] = {

    log.info(s"Handling request for DataQualityReport of Patient ${patient.value}")

    db.dataQcReportOf(patient)

  }


}
