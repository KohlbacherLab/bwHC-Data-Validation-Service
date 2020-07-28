package de.bwhc.mtb.data.entry.db


import java.io.File

import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.ekut.tbi.repo.AsyncRepository
import de.ekut.tbi.repo.fs.AsyncFSBackedInMemRepository

import de.bwhc.mtb.data.entry.dtos._
import de.bwhc.mtb.data.entry.api.DataQualityReport
import de.bwhc.mtb.data.entry.impl.{
  MTBDataDB,
  MTBDataDBProvider
}



class MTBDataDBProviderImpl extends MTBDataDBProvider
{

  def getInstance: MTBDataDB = {

    val dataDir =
      Option(System.getProperty("bwhc.data.entry.dir"))
        .map(new File(_))
        .get

    val mtbfileDB: AsyncRepository[MTBFile,Patient.Id] =
      AsyncFSBackedInMemRepository(
        dataDir,
        "MTBFile",
        _.patient.id,
        _.value
      )

    val dataReportDB: AsyncRepository[DataQualityReport,Patient.Id] =
      AsyncFSBackedInMemRepository(
        dataDir,
        "DataQualityReport",
        _.patient,
        _.value
      )

    new MTBDataDBImpl(mtbfileDB,dataReportDB)

  }

}



class MTBDataDBImpl
(
  val mtbfileDB: AsyncRepository[MTBFile,Patient.Id],
  val dataReportDB: AsyncRepository[DataQualityReport,Patient.Id],
)
extends MTBDataDB
{

  def save(
    mtbfile: MTBFile
  )(
    implicit ec: ExecutionContext
  ): Future[MTBFile] = {
    mtbfileDB.save(mtbfile)
  }

  def mtbfile(
    id: Patient.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]] = {
    mtbfileDB.get(id)
  }

  def delete(
    id: Patient.Id,
  )(
    implicit ec: ExecutionContext
  ): Future[Option[MTBFile]] = {
    mtbfileDB.delete(id)
  }


  def save(
    report: DataQualityReport
  )(
    implicit ec: ExecutionContext
  ): Future[DataQualityReport] = {
    dataReportDB.save(report)
  }

  def dataQcReports(
    implicit ec: ExecutionContext
  ): Future[Iterable[DataQualityReport]] = {
    dataReportDB.query(_ => true)
  }

  def dataQcReportOf(
    patId: Patient.Id
  )(
    implicit ec: ExecutionContext
  ): Future[Option[DataQualityReport]] = {
    dataReportDB.get(patId)
  }

}
