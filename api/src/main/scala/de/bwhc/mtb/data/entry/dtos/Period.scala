package de.bwhc.mtb.data.entry.dtos



import java.time.temporal.Temporal

import play.api.libs.json.{Format,Json}


sealed trait Period[T <: Temporal]
{ 
  val start: T
}


final case class ClosedPeriod[T <: Temporal]
(
  start: T,
  end: T
) extends Period[T]

object ClosedPeriod
{
  implicit def format[T <: Temporal: Format] = Json.format[ClosedPeriod[T]]
}


final case class OpenEndPeriod[T <: Temporal]
(
  start: T,
  end: Option[T]
) extends Period[T]

object OpenEndPeriod
{

  def apply[T <: Temporal](
    start: T
  ): OpenEndPeriod[T] =
    OpenEndPeriod(start,None)


  implicit def format[T <: Temporal: Format] =
    Json.format[OpenEndPeriod[T]]
}

