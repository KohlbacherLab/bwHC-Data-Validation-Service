package de.bwhc.mtb.data.entry.views


import java.time.temporal.Temporal

import play.api.libs.json.{Json,Format}

import de.bwhc.mtb.data.entry.dtos.{
  Period,
  ClosedPeriod,
  OpenEndPeriod
}


final case class PeriodDisplay[T <: Temporal](value: String) extends View[Period[T]]


object PeriodDisplay
{
  implicit def format[T <: Temporal: Format] = Json.writes[PeriodDisplay[T]]

  implicit def dflt[T <: Temporal] = Default(PeriodDisplay[T]("N/A"))
}

