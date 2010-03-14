package scapps

import scalaz._
import Scalaz._

import belt._

trait Validations {  
  def nonEmpty(msg: String = "should not be empty")(str: String): Option[NonEmptyList[String]] = some(str).filter(_.isEmpty) map (_ => msg.wrapNel)
  
  //(A => Option[Nel[E]]) => (B => Option[Nel[E]])
  
  def matches(r: scala.util.matching.Regex, msg: String = "should be valid")(str: String): Option[NonEmptyList[String]] = {
    r.findFirstMatchIn(str) some { _ => none[NonEmptyList[String]] } none { some(msg.wrapNel) }
  }
}
