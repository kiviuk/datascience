package datascience

import org.scalatest.{Matchers, FunSpec}

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import scala.util.{Success, Try}

@RunWith(classOf[JUnitRunner])
class TwitterClientTest extends FunSpec with Matchers with MockFactory  {
  describe("Twitter client") {

    it("should construct") {
      new TwitterClient()
    }

    it("should connect with google.com") {
      val tc = new TwitterClient()
      val responseTuple = tc.get("https://www.google.com") getOrElse( (0,"")  )
      responseTuple._1 should be (200)
    }

    it("should connect with a fake account") {

      val mockTwitterClient = mock[TwitterClient]
      (mockTwitterClient.get _).expects("https://www.google.com").returning(Try(100, "test"))

      assert(mockTwitterClient.get("https://www.google.com") === Try(100, "test"))
    }

  }

}
