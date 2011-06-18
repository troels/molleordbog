package org.bifrost.molleordbog

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

class MolleOrdbog extends HttpServlet { 
  override def doGet(request: HttpServletRequest, response: HttpServletResponse) {
    response.setContentType("text/html")
    response.getWriter.println("hello there")
  }
}
