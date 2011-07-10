package org.bifrost.molleordbog.tests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConversions._

import org.bifrost.molleordbog.dbcreator.ExtractItems
import org.bifrost.molleordbog.model.{Synonym, Model}
import org.bifrost.utils.templates.OurCounterfeiter
import org.bifrost.utils.U._
import org.bifrost.utils.http.MockHttpRequest

import org.bifrost.molleordbog.RequestHandler

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.google.appengine.tools.development.testing.LocalServiceTestHelper

class SimpleTest extends FunSuite with ShouldMatchers with BeforeAndAfterAll { 
  val helper = new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig)
  
  override def beforeAll { 
    helper.setUp();
    ExtractItems.collectWordsInDb()
  }
  override def afterAll { helper.tearDown() }
  
  test("Loading counterfeiter") { 
    OurCounterfeiter.renderTemplate("main.search", map = Map("word" -> "hi")) should startWith (
      "<!doctype html>\n<html>")
  }

  test("Test finding words in import") { 
    val query = Model.obj query classOf[Synonym] filter ("word > ", "kor\u0000") filter 
                        ("word < ", "kor\uFFFF") toList

    query should have size 5
  }

  test("Autocomplete query") { 
    val mockReq = new MockHttpRequest(uri="/autocomplete/", args=Map("word" -> List("kor")))
    val res = new String(RequestHandler handle mockReq content, "utf-8")
    res should equal (List("kornet hænger", "kornmølle", "kors", "korte krøjestivere", "korte stivere") mkString "\n")
  }

  test("Showword template") { 
    println(OurCounterfeiter.renderTemplate("main.showWord", map = Map("article" -> "hello")))
  }
}
