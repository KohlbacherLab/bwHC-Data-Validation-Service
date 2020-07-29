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



trait QueryServiceProxy
{

  def process(
    cmd: QueryServiceProxy.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,QueryServiceProxy.Response]] 

  def !(cmd: QueryServiceProxy.Command)(implicit ec: ExecutionContext) = process(cmd) 

}


trait QueryServiceProxyProvider extends SPI[QueryServiceProxy]

object QueryServiceProxy extends SPILoader(classOf[QueryServiceProxyProvider])
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

