package datascience

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, FunSpec}
import org.scalamock.scalatest.MockFactory
import datascience.Perceptron._
import grizzled.math.stats._

@RunWith(classOf[JUnitRunner])
class PerceptronTest extends FunSpec with Matchers with MockFactory  {
  describe("Perceptron") {

    it("should multiply tuples") {
      assert ( (1,2,3) * (2,3,4) == 2 + 6 + 12 )
    }

    it("should add tuples") {
      assert ( (1,2,3) + (2,3,4) == (3,5,7) )
    }

    it("should multiply scalar and a tuple") {
      assert ( (1,2,3) * 4 == (4,8,12))
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
      assert(trainingDataFunction(n).size == n)
    }

    it("should return a list of all misclassified points with regard to historical data") {
      val n = 100
      val w = (0.0, 0.0, 0.0)
      val misClassifiedPointsCount = findMisclassifiedPoints(trainingDataFunction(n))(w).size

      assert(misClassifiedPointsCount == n)
    }

    it("should return a misclassified point") {
      val n = 100
      val w = (0.0, 0.0, 0.0)
      val mcps = findMisclassifiedPoints(trainingDataFunction(n))(w)

      val misClassifiedPoint = randomlyPickMisClassifiedPoint(mcps)

      assert (misClassifiedPoint.isDefined)

      println(misClassifiedPoint.get)

    }

    it("should loop uninterrupted") {
      var x = 0; var N = 1
      val unconditionalLoop = loop(N)(cont = true)_

      val looped = unconditionalLoop(x += 2)
      assert(looped == N)
      assert(x == 2 * looped)
    }

    it("should loop while condition holds true") {
      var x = 0; var N = 1
      val looped = loopWhile(x<N)(x += 1)
      assert(looped == N)
    }

    it("should start startPerceptron") {
      val (loopCount, error) = startPerceptron(maxTrainingDataPoints = 10)
      assert (loopCount > 0)
      assert (error >= 0)
    }

    it("should measure startPerceptron") {
      val (avrgIterations, avrgError) = runPerceptron(runsN = 10, maxTrainingDataPoints = 100)
      println(s"avrgIterations = [$avrgIterations] avrgError = [$avrgError]")
    }


  }
}
