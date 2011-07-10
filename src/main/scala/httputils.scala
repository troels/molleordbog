package org.bifrost.utils.http

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import scala.collection.JavaConversions._
import scala.collection.mutable.{ HashMap => MHashMap }
import org.bifrost.utils.U._

object HttpMethod { 
  val httpMethods = List(GET, POST)

  def fromString(method: String): Option[HttpMethod] =
    httpMethods find { _.toString.toUpperCase == method.toUpperCase }
}

abstract class HttpMethod

object GET extends HttpMethod {
  override def toString = "GET"
}

object POST extends HttpMethod {
  override def toString = "GET"
}

abstract class Request {
  def getHeader(key: String): Option[String]
  def getHeaders(key: String): Iterator[String]
  def method: HttpMethod
  def uri: String
  
  def getArg(key: String): Option[String]
  def getArgs(key: String): List[String]
  
  def getSessionAttribute(key: String): Option[AnyRef]
  def putSessionAttribute(key: String, value: AnyRef): Unit 
   
  def getRequestAttribute(key: String): Option[AnyRef] 
  def putRequestAttribute(key: String, value: AnyRef): Request
}

abstract class Response {
  def headers: List[(String, String)]
  def content: Array[Byte]

  def statusCode: Int
  def statusLine: Option[String] = None
  
  def outputHeaders(resp: HttpServletResponse) { 
    headers foreach {case (k, v) => resp.addHeader(k, v)  }
  }    
  def toServletResponse(resp: HttpServletResponse) { 
    statusLine match { 
      case Some(line) => resp.sendError(statusCode, line)
      case None => resp.setStatus(statusCode)
    }
    
    outputHeaders(resp)

    val stream = resp.getOutputStream
    try { stream.write(content) } finally { stream.close() }
  }
}

class HttpRequest(request: HttpServletRequest, requestAttributes: Map[String, AnyRef] = Map()) extends Request {
  override def getHeaders(key: String) = (request getHeaders key).asInstanceOf[java.util.Enumeration[String]]
  override def getHeader(key: String) = request getHeader key

  override def method = HttpMethod fromString (request getMethod) get
  override def uri = request getRequestURI
  
  lazy val arg: Map[String, List[String]] = 
    (request getParameterMap).asInstanceOf[java.util.Map[String, Array[String]]] mapValues ( _ toList ) toMap
  
  override def getArg(key: String) = getArgs(key) headOption
  override def getArgs(key: String) = arg get key getOrElse List()

  lazy val session = request getSession
  
  override def getSessionAttribute(key: String) = session getAttribute key
  override def putSessionAttribute(key: String, value: AnyRef) = session.setAttribute(key, value)
  
  override def getRequestAttribute(key: String) = requestAttributes get key
  override def putRequestAttribute(key: String, value: AnyRef) = 
    new HttpRequest(request, requestAttributes + (key -> value))
}

class MockHttpRequest(
  val uri: String,
  override val method: HttpMethod = GET,
  headers: Map[String, List[String]] = Map(),
  args: Map[String, List[String]] = Map(),
  session: MHashMap[String, AnyRef] = MHashMap(), 
  attributes: Map[String,AnyRef] = Map()) extends Request {
    override def getHeaders(key: String) = headers getOrElse (key, List()) toIterator
    override def getHeader(key: String) = headers getOrElse (key, List()) headOption
    
    override def getArg(key: String) = getArgs(key) headOption
    override def getArgs(key: String) = args get key getOrElse List()
    
    override def getSessionAttribute(key: String) = session get key
    override def putSessionAttribute(key: String, value: AnyRef) { session(key) = value }
    
    override def getRequestAttribute(key: String) = attributes get key
    override def putRequestAttribute(key: String, value: AnyRef) = 
      new MockHttpRequest(uri=uri, method=method, headers=headers, args=args, 
                          session=session, attributes=attributes + (key -> value))
}
  
class HttpResponse(contentString: String, contentType: String, encoding: String = "UTF-8",
                   inputHeaders: List[(String, String)] = List(), val statusCode: Int = 200, 
                   override val statusLine: Option[String] = None) extends Response {
  override def content: Array[Byte] = contentString getBytes encoding
  override def headers = inputHeaders :+ ("Content-Type", "%s; charset=%s" format (contentType, encoding))
}

object HttpRedirectResponse { 
  def apply(destination: String) = new HttpRedirectResponse(destination)
}

class HttpRedirectResponse(destinationUrl: String, val statusCode: Int = 301) extends Response {
  def headers : List[(String, String)] = List()
  def content: Array[Byte] = null
  
  override def toServletResponse(resp: HttpServletResponse) { 
    outputHeaders(resp)
    resp.setStatus(statusCode)
    resp.sendRedirect(resp.encodeRedirectURL(destinationUrl))
  }
}

object HttpRequestErrorResponse { 
  def apply(statusLine: Option[String] = None) = new HttpRequestErrorResponse(statusLine)
  def defaultError = "Request was not complete"
}

class HttpRequestErrorResponse(statusLine: Option[String] = None) extends HttpResponse(
  contentString = statusLine getOrElse HttpRequestErrorResponse.defaultError, 
  contentType = "text/plain",  
  statusCode = 400,
  statusLine = statusLine orElse HttpRequestErrorResponse.defaultError)

object HtmlResponse { 
  def apply(str: String, statusCode: Int = 200, statusLine: Option[String] = None) = 
    new HtmlResponse(str, statusCode = statusCode, statusLine = statusLine) 
}

class HtmlResponse(contentString: String, statusCode: Int= 200, statusLine: Option[String] = None) extends 
     HttpResponse(contentString, "text/html", statusCode = statusCode, statusLine = statusLine)

object TextResponse { 
  def apply(contentString: String) = 
    new TextResponse(contentString)
}

class TextResponse(contentString: String, statusCode: Int = 200, statusLine: Option[String] = None) extends 
     HttpResponse(contentString, "text/plain", statusCode=statusCode, statusLine=statusLine)