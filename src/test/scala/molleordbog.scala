package org.bifrost.molleordbog.tests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConversions._

import org.bifrost.molleordbog.dbcreator.ExtractItems
import org.bifrost.molleordbog.model.{Synonym, Model}

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig
import com.google.appengine.tools.development.testing.LocalServiceTestHelper

class SimpleTest extends FunSuite with ShouldMatchers with BeforeAndAfter { 
  val helper = new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig)
  
  before { 
    helper.setUp();
    ExtractItems.collectWordsInDb()
  }
  after { helper.tearDown() }

  test("Test finding words in import") { 
    val query = Model.obj query classOf[Synonym] filter ("word > ", "kor\u0000") filter 
                        ("word < ", "kor\uFFFF") toList

    query 
  }
}
