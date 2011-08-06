package org.bifrost.utils.http

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import scala.collection.JavaConversions._
import scala.collection.mutable.{ HashMap => MHashMap }
import org.bifrost.utils.U._


import com.google.appengine.api.blobstore.BlobstoreServiceFactory
import com.google.appengine.api.blobstore.BlobKey

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
   
  def getRequestAttribute(key: String): Option [AnyRef] 
  def putRequestAttribute(key: String, value: AnyRef): Request
  
  def originalRequest: Option[HttpServletRequest] = None

  def isAjax: Boolean = 
    getHeader("X-Requested-With") map { _.toLowerCase == "xmlhttprequest" } getOrElse false 
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
    try { stream.write(content) } finally { try { stream.close() } catch { case e: java.io.IOException => } }
  }
}

class HttpRequest(request: HttpServletRequest, requestAttributes: Map[String, AnyRef] = Map()) extends Request {
  override def getHeaders(key: String) = (request getHeaders key).asInstanceOf[java.util.Enumeration[String]]
  override def getHeader(key: String) = safelyNullable(request getHeader key)

  override def method = HttpMethod fromString (request getMethod) get
  override def uri = request getRequestURI
  
  val arg: Map[String, List[String]] = {
    val params = request getParameterNames

    val names: List[String] = (params toList) map { _.toString }
    
    names map { 
      name => (name -> (request getParameterValues name toList))
    } toMap
  }  
  
  override def getArg(key: String) = getArgs(key) headOption
  override def getArgs(key: String) = arg get key getOrElse List()

  lazy val session = request getSession
  
  override def getSessionAttribute(key: String) = safelyNullable(session getAttribute key)
  override def putSessionAttribute(key: String, value: AnyRef) = session.setAttribute(key, value)
  
  override def getRequestAttribute(key: String) = requestAttributes get key
  override def putRequestAttribute(key: String, value: AnyRef) = 
    new HttpRequest(request, requestAttributes + (key -> value))

  override def originalRequest = Some(request)
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
  
class HttpResponse(contentString: String, contentType: String, encoding: Option[String] = None,
                   inputHeaders: List[(String, String)] = List(), val statusCode: Int = 200, 
                   override val statusLine: Option[String] = None) extends Response {
  override def content: Array[Byte] = contentString getBytes (encoding getOrElse "UTF-8")
  override def headers = inputHeaders :+ (
    "Content-Type", if (encoding isEmpty) contentType else "%s;charset=%s" format (contentType, encoding))
}

object RedirectResponse { 
  def apply(destination: String) = new RedirectResponse(destination)
}

class RedirectResponse(destinationUrl: String, val statusCode: Int = 301) extends Response {
  def headers : List[(String, String)] = List()
  def content: Array[Byte] = null
  
  override def toServletResponse(resp: HttpServletResponse) { 
    outputHeaders(resp)
    resp.setStatus(statusCode)
    resp.sendRedirect(resp.encodeRedirectURL(destinationUrl))
  }
}

object RequestErrorResponse { 
  def apply(statusLine: Option[String] = None) = new RequestErrorResponse(statusLine)
  def defaultError = "Request was not complete"
}

class RequestErrorResponse(statusLine: Option[String] = None) extends HttpResponse(
  contentString = statusLine getOrElse RequestErrorResponse.defaultError, 
  contentType = "text/plain",  
  statusCode = 400,
  statusLine = statusLine orElse safelyNullable(RequestErrorResponse.defaultError))

object HtmlResponse { 
  def apply(str: String, statusCode: Int = 200, statusLine: Option[String] = None) = 
    new HtmlResponse(str, statusCode = statusCode, statusLine = statusLine) 
}

class HtmlResponse(contentString: String, statusCode: Int= 200, statusLine: Option[String] = None) extends 
     HttpResponse(contentString, "text/html; charset=UTF-8", statusCode = statusCode, statusLine = statusLine)

object TextResponse { 
  def apply(contentString: String) = 
    new TextResponse(contentString)
}

class TextResponse(contentString: String, statusCode: Int = 200, statusLine: Option[String] = None) extends 
     HttpResponse(contentString, "text/plain", statusCode=statusCode, statusLine=statusLine)

object JSONResponse { 
  def apply(json: AnyRef) = new JSONResponse(json)
}

class JSONResponse(json: AnyRef, statusCode: Int = 200, statusLine: Option[String] = None) extends
     HttpResponse(gson toJson json , "application/json", statusCode=statusCode, statusLine=statusLine)

object BlobResponse { 
  def apply(req: HttpServletRequest, contentType: String, blobKey: BlobKey, fileName: Option[String] = None) = 
    new BlobResponse(req, contentType, blobKey, fileName)
}

class BlobResponse(req: HttpServletRequest, contentType: String, blobKey: BlobKey, fileName: Option[String]=None) 
extends HttpResponse("", contentType) {
  def asciify(str: String): String = 
    str.replaceAll("ø", "oe").replaceAll("Ø", "OE").replaceAll("Æ", "AE").replaceAll("å", "aa")
       .replaceAll("Å", "AA").replaceAll("æ", "ae")
  
  override def headers = super.headers ++ (
    fileName map { fn => ("Content-Disposition" -> ("attachment; filename=%s" format (asciify(fn)))) } toList
  )
  
  override def toServletResponse(resp: HttpServletResponse) {
    resp.setStatus(statusCode)
    
    outputHeaders(resp)
    
    val blobstoreService = BlobstoreServiceFactory getBlobstoreService
    val byteRange = blobstoreService getByteRange req
    blobstoreService serve (blobKey, byteRange, resp)
  }
}
