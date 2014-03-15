package datascience

import org.apache.commons.httpclient.{HttpConnectionManager, SimpleHttpConnectionManager, HttpClient}
import org.apache.commons.httpclient.methods.GetMethod
import scala.util.Try

case class TwitterClient() {

  val client = new HttpClient(new SimpleHttpConnectionManager())
  client.getHttpConnectionManager.getParams.setConnectionTimeout(30000)

  /*
      http://danielwestheide.com/blog/2012/12/26/the-neophytes-guide-to-scala-part-6-error-handling-with-try.html
   */
  def get(url: String): Try[(Int, String)] = Try{


    val get = new GetMethod(url)
    get.setFollowRedirects(true)

    val iGetResultCode = client.executeMethod(get)
    val response: String = get.getResponseBodyAsString

    get.releaseConnection();


    (iGetResultCode, response)

  }
  
}
