package org.bifrost.molleordbog

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import org.bifrost.utils.http._
import org.bifrost.utils.mapping._
import org.bifrost.molleordbog.views._

import com.google.appengine.api.memcache._
import com.google.appengine.api.blobstore.BlobstoreServiceFactory

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
    val ms = MemcacheServiceFactory getMemcacheService
    val req = new HttpRequest(request)
    
    val shouldCache = false //req.method == GET
    val mini = req miniRequest 
    var cached = if (shouldCache) { ms.get(mini) } else { null } 
    
    if (cached == null) { 
      val resp = RequestHandler handle req
      if (shouldCache) { ms.put(mini, resp) }
      cached = resp.asInstanceOf[AnyRef]
    }
    cached.asInstanceOf[Response].toServletResponse(request, response)
  }
}
