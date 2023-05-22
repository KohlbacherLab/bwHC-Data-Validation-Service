package de.bwhc.mtb.data.entry

package object views 
{


  import scala.util.Either

  import cats.syntax.either._

  import play.api.libs.json.{Json,Format,JsString,Writes}


  type Or[+T,+U] = Either[T,U]


  sealed trait NoValue

  sealed trait NotAvailable extends NoValue
  final case object NotAvailable extends NotAvailable
  {
    implicit val format: Writes[NotAvailable] =
      Writes(na => JsString("N/A"))
  }

  sealed trait Undefined extends NoValue
  final case object Undefined extends Undefined
  {
    implicit val format: Writes[Undefined] =
      Writes(u => JsString("-"))
  }

  object NoValue
  {
    implicit val format: Writes[NoValue] =
      Writes {
        case na: NotAvailable => Json.toJson(na)
        case ud: Undefined    => Json.toJson(ud)
      }
  } 



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

/*
  import scala.language.implicitConversions

  implicit def yesOrNofromBoolean(b: Boolean): Yes Or No =
    if (true) Yes.asLeft[No] else No.asRight[Yes]
*/



  final case class Default[T](value: T) extends AnyVal
  object Default
  {
    def valueOf[T](implicit dflt: Default[T]): T = dflt.value
  }


  final case class DatedValue[T,V]
  (
    date: T,
    value: V
  )

  object DatedValue
  {
    implicit def formatDatedValue[T: Format, V: Format] =
      Json.format[DatedValue[T,V]]
  }



}
