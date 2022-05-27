package de.bwhc.mtb.data.entry.dtos



import java.net.URI

import play.api.libs.json.{
  Format, Reads, Writes, JsPath, JsString, JsObject, JsValue
}


final case class Coding[C: Coding.System]
(
  code: C,
  display: Option[String],
  version: Option[String]
)
{
  def withDisplay(implicit valueSet: ValueSet[C]) =
    copy(
      display = valueSet.displayOf(code)
    )
}


object Coding
{


  def apply[C: Coding.System](
    code: C,
    display: Option[String]
  ): Coding[C] =
    Coding(code,display,None)


  def apply[C: Coding.System: ValueSet](
    code: C
  ): Coding[C] =
    Coding(code,ValueSet[C].displayOf(code))
    
/* 
  implicit def ordering[C](implicit ord: Ordering[C]): Ordering[Coding[C]] =
    new Ordering[Coding[C]]{
      override def compare(x: Coding[C], y: Coding[C]): Int =
        ord.compare(x.code,y.code)
    }
*/

  @annotation.implicitNotFound("Couldn't find Coding.System instance for ${C}")
  sealed trait System[C]{
    val uri: URI
    override def toString = uri.toString
  }
  object System
  {
    def apply[C](implicit sys: System[C]): System[C] = sys

    def apply[C](sys: URI): System[C] = new System[C]{ val uri = sys }
    def apply[C](sys: String): System[C] = System[C](URI.create(sys))
  }


  import play.api.libs.functional.syntax._

  implicit def format[C](
    implicit
    system: Coding.System[C],
    fc: Format[C]
  ): Format[Coding[C]] = {

    val read: Reads[Coding[C]] =
      (
        (JsPath \ "code").read[C] and
        (JsPath \ "display").readNullable[String] and
        (JsPath \ "version").readNullable[String]
      )(Coding.apply[C](_,_,_))

    val write: Writes[Coding[C]] =
      (
        (JsPath \ "code").write[C] and
        (JsPath \ "display").writeNullable[String] and
        (JsPath \ "version").writeNullable[String]
      )(unlift(Coding.unapply[C]))

    Format[Coding[C]](
      read,
      write.transform(_.as[JsObject] + ("system" -> JsString(system.uri.toString)))
    )
  }

}
