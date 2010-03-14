package scapps

import hprops._
import scalaz.http.request._
import scalaz._
import Scalaz._

import metascala.HLists._

case class PostableProperty[T](str: String, validations: T => Option[NonEmptyList[String]])(implicit p: Postable[T]) extends ReadUpdate[Request[Stream], T] {
  
  def validate(t: T) = validations(t).toFailure(t).fail.map(_ map (CustomError(str,_))).validation
  
  def read(r: Request[Stream]) = (r |! str).map(_.mkString).toSuccess(missing(str).wrapNel) >>= (v => p.read(v).toSuccess(invalid(str).wrapNel)) >>= validate
  
  def update(r: Request[Stream], t: T) = (read(r).fail.map(_.list) >>= {
    case Missing(_) :: Nil => success(t).fail
    case a => failure(a.toNel.get).fail
  } validation) >>= validate
  
  def where(f: T => Option[NonEmptyList[String]]) = this copy (validations = f)
}

trait StringW {
  val str: String
  
  import hprops._
  
  def as[T](implicit p: Postable[T]) = PostableProperty[T](str, (_ => none))
  
  def params(ts: (Symbol, String)*) = {
    val parameters: String = ts.map { case (a,b) => a.toString + "=" + b }.mkString("&")
    "%s?%s" format (str, parameters)
  }
  
  def asType[T, U <: NewType[T]](f: T => U)(implicit m: ClassManifest[T], p: Postable[T]) = 
    as[T] >< (f, ((_: NewType[T]).value))
}

trait Postable[T] {
  def read(str: String): Option[T]
}

object Postable {
  def postable[T](f: String => Option[T]) = new Postable[T] { def read(str: String) = f(str) }
  
  implicit val string = postable[String](some _)
  implicit val int = postable[Int](_.parseInt.success)
  implicit val long = postable[Long](_.parseLong.success)
  implicit val double = postable[Double](_.parseDouble.success)
}

trait StringImplicits {
  implicit def to(s: String) = new StringW { val str = s }
  implicit def from(sw: StringW) = sw.str
}

