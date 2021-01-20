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


  object syntax
  {
 
    implicit class MappingOps[T](val t: T) extends AnyVal
    {
      def mapTo[V](implicit f: T => V) = f(t)
    }

  }


//  import java.time.temporal.Temporal

//  final case class TemporalValue[T <: Temporal,V]
  final case class TemporalValue[T,V]
  (
    date: T,
    value: V
  )

  object TemporalValue
  {
//    implicit def formatTemporalValue[T <: Temporal: Format, V: Format] =
    implicit def formatTemporalValue[T: Format, V: Format] =
      Json.format[TemporalValue[T,V]]
  }



}
