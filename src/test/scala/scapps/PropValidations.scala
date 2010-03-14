package scapps

import metascala.HLists._

import hprops._
import scalaz._
import Scalaz._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class PropValidationsSpec extends BaseSuite {
  import scalaz.http._
  import request._
  
  def failures[T, X](v: Validation[NonEmptyList[T], X]): List[T] = v.failure.map(_.list) | Nil
  
  def r(qs: String) = {
    val line = Line.line(GET, Uri.uri("/abc".charsNel.get, some(qs.toList)), Version.version11)    
    Request.request(line, Nil, Stream.empty)
  }
  
  object MyValids extends Validations 
  import MyValids._
  
  
  case class Name(name: String)
  
  val nameProp = "name".as[String].where {
    nonEmpty()_ |+| matches("[A-Z]+.*".r, msg = "not capitalised" )
  }.hlift >< (Name <-> Name.unapply _)
  
  test("reading") {
    nameProp.read(r("name=Nick")).success should equal (some(Name("Nick")))
  }
  
  test ("non empty") {
    failures(nameProp.read(r("name=&age=15"))) should equal (CustomError("name", "should not be empty") :: CustomError("name", "not capitalised") :: Nil)
  }
  
  test ("two of them") {
    failures(nameProp.read(r("name=lower"))) should equal (CustomError("name", "not capitalised") :: Nil)
  }
  
  test("updating") {
    val start = Name("Nick")
    
    nameProp.update(r("name=Ben&test=foo"), start) should equal {
      success(Name("Ben"))
    }
    
    nameProp.update(r("name=ben&test=foo"), start).failure.map(_.list) should equal {
      some(CustomError("name", "not capitalised") :: Nil)
    }
  }
}