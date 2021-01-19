package de.bwhc.mtb.data.entry.dtos




import play.api.libs.json.{Format,Json}


final case class ValueSet[C]
(
  name: String,
  concepts: List[ValueSet.Concept[C]]
)
{
  def getCode(c: C) =
    concepts.find(_.code == c)
}


object ValueSet
{

  final case class Concept[C]
  (
    code: C,
    display: String
  )


  def apply[C](implicit vs: ValueSet[C]) = vs


  implicit def formatConcept[C: Format] = Json.format[Concept[C]]

  implicit def format[C: Format] = Json.format[ValueSet[C]]


}
