package de.bwhc.mtb.data.entry.api


import java.time.Instant

import scala.util.Either

import scala.concurrent.{
  ExecutionContext,
  Future
}

import cats.data.NonEmptyList

import de.bwhc.util.ddd.Event
import de.bwhc.util.spi._

import de.bwhc.mtb.data.entry.dtos._



trait MTBDataServiceProvider extends SPI[MTBDataService]


trait MTBDataService
{

  def process(
    cmd: MTBDataService.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,MTBDataService.Response]]  //TODO: re-think better error modelling

  def !(cmd: MTBDataService.Command)(implicit ec: ExecutionContext) = process(cmd)


  def patientsWithIncompleteData(
    implicit ec: ExecutionContext
  ): Future[Iterable[Patient.Id]]


  def mtbfile(
    patient: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]]

  def dataQualityReport(
    patient: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[DataQualityReport]]

}


object MTBDataService extends SPILoader(classOf[MTBDataServiceProvider])
{

  sealed abstract class Command
  object Command
  {
    case class Upload(mtbfile: MTBFile) extends Command

    case class Delete(patient: Patient.Id) extends Command
  }

  sealed abstract class Response extends Event
  object Response
  {
    case class Imported
    (
      result: Either[DataQualityReport,MTBFile],
      timestamp: Instant = Instant.now
    ) extends Response
  
    case class Deleted(
      patient: Patient.Id,
      timestamp: Instant = Instant.now
    ) extends Response
  }

}
