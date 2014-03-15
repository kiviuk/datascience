package datascience

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, FunSpec}
import org.scalamock.scalatest.MockFactory
import datascience.Perceptron._

@RunWith(classOf[JUnitRunner])
class PerceptronTest extends FunSpec with Matchers with MockFactory  {
  describe("Perceptron") {

    it("should multiply tuples") {
      assert ( (1,2,3) * (2,3,4) == 2 + 6 + 12 )
    }

    it("should generate N equally distributed random training points") {
      val n = 10
      val r = randomTrainingPoints(n)
      assert(r.size == n)
    }

    it("should generate two unequal random points") {
      assert (randomPoint() != randomPoint())
    }

    it("should classify a random point") {
      val point = randomPoint()
      assert(classificationFunction(point) == classificationFunction(point))
      assert(classificationFunction eq classificationFunction)
    }

    it("should generate historical data") {
      val n = 10
      assert(historicalDataFunction(n).size == n)
    }
  }
}
