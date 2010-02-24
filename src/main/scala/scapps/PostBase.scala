package scapps

import belt._
import scalaz._
import Scalaz._

trait PostBase[T] extends RequestCreate[T] with RequestUpdate[T] with scapps.Validations {
  val Required = "%s is required" // TODO: Messages trait. Or something.
  
  def * : hprops.ReadUpdate[scalaz.http.request.Request[Stream], T]
  
  def handleErrors[T](r: hprops.Result[T]): Validation[NonEmptyList[(String,String)], T] = {
    r.fail.map { errs =>
      errs map {
        case hprops.Missing(s) => (s, Required format s)
        case hprops.Invalid(s) => (s, Required format s)
      }
    }.validation
  }
  
  def create(r: Request) = handleErrors(this.*.get(r.underlying))
  
  def update(r: Request)(t: T) = handleErrors(this.*.put(r.underlying, t)).fold(
    {err => (err.list, t)},
    { t => (Nil, t) }
  )
}
