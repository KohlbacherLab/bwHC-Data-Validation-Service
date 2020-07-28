package de.bwhc.mtb.data.entry.impl


import java.time.Instant

import scala.util.Either

import scala.concurrent.{
  ExecutionContext,
  Future
}

import de.bwhc.util.ddd.Event
import de.bwhc.util.spi._

import de.bwhc.mtb.data.entry.dtos._



trait QueryService
{

  def process(
    cmd: QueryService.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,QueryService.Response]] 

  def !(cmd: QueryService.Command)(implicit ec: ExecutionContext) = process(cmd) 

}


trait QueryServiceProvider extends SPI[QueryService]

object QueryService extends SPILoader(classOf[QueryServiceProvider])
{

  sealed trait Command
  object Command
  {
    final case class Upload(mtbfile: MTBFile) extends Command
    final case class Delete(patient: Patient.Id) extends Command
  }


  sealed trait Response
  object Response
  {
    final case object Imported extends Response
    final case object Deleted  extends Response
  }

}

