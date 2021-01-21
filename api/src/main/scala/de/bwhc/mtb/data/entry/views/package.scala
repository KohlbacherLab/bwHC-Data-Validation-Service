package de.bwhc.mtb.data.entry

package object views 
{


  import scala.util.Either

  import cats.syntax.either._

  import play.api.libs.json.{Json,Format,JsString,Writes}


  type Or[+T,+U] = Either[T,U]


  sealed trait NotAvailable
  final case object NotAvailable extends NotAvailable
  {
    implicit val format: Writes[NotAvailable] =
      Writes(na => JsString("N/A"))
  }


/*
  sealed trait YesOrNo
  final case object Yes extends YesOrNo
  final case object No extends YesOrNo


  object YesOrNo
  {    
    import scala.language.implicitConversions

    implicit def fromBoolean(b: Boolean): YesOrNo =
      if (true) Yes else No

    implicit val format: Writes[YesOrNo] =
      Writes {
         case Yes => JsString("Ja")
         case No  => JsString("Nein")
      }
  }
*/


  sealed trait Yes
  final case object Yes extends Yes
  {
    implicit val format = Writes { (y: Yes) => JsString("Ja") }
  }

  sealed trait No
  final case object No extends No
  {
    implicit val format = Writes { (n: No) => JsString("Nein") }
  }

  import scala.language.implicitConversions

  implicit def yesOrNofromBoolean(b: Boolean): Yes Or No =
    if (true) Yes.asLeft[No] else No.asRight[Yes]




  final case class Default[T](value: T) extends AnyVal
  object Default
  {
    def valueOf[T](implicit dflt: Default[T]): T = dflt.value
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
