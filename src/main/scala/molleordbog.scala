package org.bifrost.molleordbog

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.bifrost.utils.http._
import org.bifrost.utils.mapping._

object Views { 
  def testView(req: Request): Response = HtmlResponse("<html><head><title>Hello</title></head><body>HI</body></html>")
}

object MolleOrdbogMappings extends BaseMapping {
  lazy val mappings: Mapping = 
    ("hello" / "there" ==> Views.testView) | 
    ("hi" / (("there"  ==> Views.testView) | ("fine" ==> Views.testView))) |
    ("goodbye" / slug("hello") ==> Views.testView)
}

class MolleOrdbog extends HttpServlet { 
  override def service(request: HttpServletRequest, response: HttpServletResponse) {
    val req = new HttpRequest(request)
    val resp = MolleOrdbogMappings(req) match { 
      case Some((req, f)) => f(req)
      case None => HtmlResponse("hello")
    }

    resp.toServletResponse(response)
  }
}
