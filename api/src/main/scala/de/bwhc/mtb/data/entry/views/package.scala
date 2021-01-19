package de.bwhc.mtb.data.entry

package object views 
{


  import scala.util.Either

  import play.api.libs.json.{Json,Format}



  type Or[+T,+U] = Either[T,U]



  final case class Default[T](value: T) extends AnyVal

  object Default
  {
    def valueOf[T](implicit dflt: Default[T]): T = dflt.value
  }


  trait View[T]


  object syntax
  {
 
    implicit class ViewOps[T](val t: T) extends AnyVal
    {
       def to[V](implicit f: T => V) = f(t)

       def toOpt[V](implicit f: T => Option[V]) = f(t)
    }

  }


  import java.time.temporal.Temporal

  final case class TemporalValue[T <: Temporal,V]
  (
    date: T,
    value: V
  )

  object TemporalValue
  {
    implicit def formatTemporalValue[T <: Temporal: Format, V: Format] =
      Json.format[TemporalValue[T,V]]
  }

 
  type TimeSeries[T <: Temporal,V] = List[TemporalValue[T,V]]


}
