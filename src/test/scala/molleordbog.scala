package org.bifrost.molleordbog.tests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConversions._

import org.bifrost.molleordbog.dbcreator.ExtractItems
import org.bifrost.molleordbog.model.{Synonym, Model, SynonymGroup, Excision, Subject, VisualSearchPicture}
import org.bifrost.utils.templates.OurCounterfeiter
import org.bifrost.utils.U._
import org.bifrost.utils.http.MockHttpRequest

import org.bifrost.molleordbog.RequestHandler

import com.googlecode.objectify._

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.google.appengine.tools.development.testing.LocalServiceTestHelper

import java.io._

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

    query should have size 14
  }

  test("Test find article") { 
    (SynonymGroup findSynonymGroup "kors") should not equal None
  }

  def reqTest(uri: String, args: (String, List[String])*) =  {
    val mockReq = new MockHttpRequest(uri=uri, args=Map(args :_*))
    RequestHandler handle mockReq
  }

  def reqTestBody(uri: String, args: (String, List[String])*) = 
    new String(reqTest(uri, args: _*) content, "utf-8")
    
  test("Autocomplete query") { 

    val res = reqTestBody("/ordbog/autocomplete/", "term" -> List("kor"))
    res should equal (
      """["kornet går op i kværnen","kornet hænger","kornmølle","kornmøllen","kornsække","korntilførsel","korntønde","kors","korset","korte krøjebjælke"]""")
  }
  
  test("Opslag template") { 
    val res = reqTest("/ordbog/opslag/", "ord" -> List("kors"))
    res.statusCode should equal (200)
  }
}

class SerializationTest extends FunSuite with ShouldMatchers with BeforeAndAfterAll { 
  val helper = new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig)

  override def beforeAll { helper.setUp() }
  override def afterAll { helper.tearDown() }

  test("add object") { 
    val pic = new VisualSearchPicture()
    pic.pictureName = "hello"
    pic.pictureKey = null

    pic.subjects = List(Subject("hello", List(new Key(classOf[SynonymGroup], "hello")) toArray)) toArray
    
    pic.excisions = List(Excision(0,0,0,0, new Key(classOf[VisualSearchPicture], "hello"))) toArray

    val key = pic.save()
    
    VisualSearchPicture get (pic pictureName) 
  }
}
