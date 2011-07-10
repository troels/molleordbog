package org.bifrost.molleordbog

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import org.bifrost.utils.http._
import org.bifrost.utils.mapping._
import org.bifrost.molleordbog.views._

object RequestHandler { 
  def handle(req: Request): Response  = {
    MolleOrdbogMappings(req) match { 
      case Some((req, f)) => f(req)
      case None => HtmlResponse("hello")
    }
  }
}

class MolleOrdbog extends HttpServlet { 
  override def service(request: HttpServletRequest, response: HttpServletResponse) {
    val req = new HttpRequest(request)
    val resp = RequestHandler handle req
    resp.toServletResponse(response)
  }
}
