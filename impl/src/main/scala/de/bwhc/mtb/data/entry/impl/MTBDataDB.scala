package de.bwhc.mtb.data.entry.impl



import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.bwhc.util.spi._

import de.bwhc.mtb.dtos._
import de.bwhc.mtb.data.entry.api.DataQualityReport


trait MTBDataDB
{

  def save(
    mtbfile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[MTBFile]


  def patients(
    implicit ec: ExecutionContext
  ): Future[Iterable[Patient]]

/*
  def mtbfiles(
    implicit ec: ExecutionContext
  ): Future[Iterable[MTBFile]]
*/

  def mtbfile(
    id: Patient.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]]


  def deleteAll(
    id: Patient.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]]



  def save(
    dataQC: DataQualityReport
  )(
    implicit ec: ExecutionContext
  ): Future[DataQualityReport]

  def dataQcReports(
    implicit ec: ExecutionContext
  ): Future[Iterable[DataQualityReport]]

  def dataQcReportOf(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[DataQualityReport]]


}


trait MTBDataDBProvider extends SPI[MTBDataDB]

object MTBDataDB extends SPILoader[MTBDataDBProvider]

