package datascience

import util.Random._
import scala.None

import Numeric._
import grizzled.math.stats._

object Perceptron {

  implicit class TupleOps[A: Numeric](t: (A, A, A)) {
    import Numeric.Implicits._
    // inner product: tuple * tuple
    def * (p: (A, A, A)) = {
      val v1 = p._1 * t._1
      val v2 = p._2 * t._2
      val v3 = p._3 * t._3
      v1 + v2 + v3
    }
    // tuple + tuple
    def + (p: (A, A, A)) = {
      val v1 = p._1 + t._1
      val v2 = p._2 + t._2
      val v3 = p._3 + t._3
      (v1, v2, v3)
    }
    // scalar product: tuple * scalar
    def * (p:A) = {
      val v1 = p * t._1
      val v2 = p * t._2
      val v3 = p * t._3
      (v1, v2, v3)
    }
  }

  def loopWhile = loop(Int.MaxValue)_
  def loop(n: Int)(cont: => Boolean)(action : => Unit): Int = {
    var loopCount = 0
    while(cont && loopCount < n) {
      action
      loopCount += 1
    }
    loopCount
  }

  // Assume X = [-1;1] x [-1;1] with uniform probability of picking each x € X
  val randomPoint = () => {
    def randomSign: Int = Vector(-1,1)(nextInt(2))
    def randomDouble = randomSign * nextDouble()
    (randomDouble, randomDouble)
  }

  // In each run, choose a random line in the plane as your target function f (do this by taking two random, uniformly distributed points
  // in [1;-1] x [-1;1] and taking the line passing through them) ...
  val randomPoint1 = randomPoint()
  val randomPoint2 = randomPoint()
  val m = {
    val x1 = randomPoint1._1
    val y1 = randomPoint1._2
    val x2 = randomPoint2._1
    val y2 = randomPoint2._2
    (y2 - y1) / (x2 - x1)
  }
  val b = {
    val x1 = randomPoint1._1
    val y1 = randomPoint1._2
    y1 - m * x1
  }
  val targetFunctionF = (x:Double) => {
    x * m + b
  }
  
  //... , where one side of the line maps to +1 and the other maps  to  1.
  val classificationFunction = (randomPoint:(Double,Double)) => {
    val fx = targetFunctionF(randomPoint._1)
    val y = randomPoint._2
    if (y>=fx) 1.0 else -1.0
  }

  // Choose the inputs xn of the data set as random points (uniformly in X), and evaluate the target function on each xn to get the corresponding output yn
  val randomTrainingPoints: Int => Vector[(Double,Double)] = (n: Int) => Stream.continually(randomPoint()).take(n).toVector
  val trainingDataFunction = (n: Int) => randomTrainingPoints(n).map( (x:(Double,Double)) => (x,classificationFunction(x)) )
  val testDataFunction = (n: Int) => randomTrainingPoints(n).map( (x:(Double,Double)) => (x,classificationFunction(x)) )

  val findMisclassifiedPoints = (hd: Vector[((Double, Double), Double)]) => (weightVector:(Double, Double, Double)) => hd.filter{ trainingData =>
    val (trainingDataPoint, actualClassification) = trainingData
    val x0 = 1.0
    val (x1, x2) = trainingDataPoint
    val learnedClassification = math.signum( weightVector * (x0, x1, x2) )
    learnedClassification != actualClassification
  }

  val randomlyPickMisClassifiedPoint: Vector[((Double, Double), Double)] => Option[((Double, Double), Double)] = misClassifiedPoints => {
    val randomIndex = if ( ! misClassifiedPoints.isEmpty ) nextInt(misClassifiedPoints.size) else -1
    if (misClassifiedPoints.isDefinedAt(randomIndex)) Some(misClassifiedPoints(randomIndex)) else None
  }

  val assertCorrectness = (weightVector:(Double, Double, Double), hd: Vector[((Double, Double), Double)]) => {
    hd.foreach{ trainingData =>
      val (trainingDataPoint, actualClassification) = trainingData
      val x0 = 1.0
      val (x1, x2) = trainingDataPoint
      val learnedClassification = math.signum( weightVector * (x0, x1, x2) )
      assert( learnedClassification == actualClassification )
    }
  }

  def testHypothesis:((Double, Double, Double), Vector[((Double, Double), Double)]) => Double = (weightVector, testData) => {

    var mismatches = 0

    testData.foreach{ x =>
      val (testDataPoint, actualClassification) = x

      val x0 = 1.0
      val (x1, x2) = testDataPoint

      val learnedClassification = math.signum( weightVector * (x0, x1, x2) )

      if(actualClassification != learnedClassification) {
        mismatches += 1
      }
    }

    val avrgError = mismatches.toDouble / testData.size.toDouble

    avrgError
  }

  def startPerceptron(maxTrainingDataPoints: Int): (Int, Double) = {

    var continue: Boolean = true
    var weightVector: (Double, Double, Double) = (0,0,0)
    val trainingData = trainingDataFunction(maxTrainingDataPoints)
    val testData = testDataFunction(maxTrainingDataPoints)

    val loopCount = loopWhile(continue) {

      val misClassifiedPoints = findMisclassifiedPoints(trainingData)(weightVector)
      val misClassifiedPoint = randomlyPickMisClassifiedPoint(misClassifiedPoints)

      // http://www.youtube.com/watch?feature=player_detailpage&v=mbyG85GZ0PI#t=1398
      misClassifiedPoint match {
        case Some(x) => val (misclassPoint, actualClassification) = x; weightVector = weightVector + ((1.0, misclassPoint._1, misclassPoint._2) * actualClassification)
        case None => continue = false
      }

    }
    
    // assertCorrectness(weightVector, historicalData)
    
    val error = testHypothesis(weightVector, testData)

    (loopCount, error)
  }

  def runPerceptron(runsN: Int, maxTrainingDataPoints: Int): (Double, Double) = {
    val iterations = new Array[Double](runsN)
    val errors = new Array[Double](runsN)

    ( 0 to runsN - 1 ).foreach{i =>
      val (loopCount, error) = startPerceptron(maxTrainingDataPoints)
      iterations(i) = loopCount.toDouble
      errors(i) = error
    }

    val avrgIterations = iterations.sum / runsN
    val avrgError = errors.sum / runsN

    (avrgIterations, avrgError)
  }

  def main(args: Array[String]) {
    val (avrgIterations, avrgError) = runPerceptron(runsN = 1000, maxTrainingDataPoints = 1000)

    println(f"avrgIterations = [$avrgIterations%2.2f] avrgError = [$avrgError%2.2f]")
  }

}

  // True Function
  // val x = { (_:Int) + 1 }
  // val x = (_:Int) + 1
  // val x = (y:Int) => y  + 1
  // x: Int => Int = <function1>


  // Method returns function
  // def x = { (_:Int) + 1 }
  // def x = (_:Int) + 1
  // def x = (y:Int) => y  + 1
  // x: Int => Int


  /////////////////////////////
  // Function values
  /////////////////////////////

  // http://stackoverflow.com/questions/4697404/scala-currying-by-nested-functions-or-by-multiple-parameter-lists?rq=1
  // Scala provides a number of different syntactic options for declaring function values. For example, the following declarations are exactly equivalent:

  // Using type inference
  // val f1 = ((a: Int, b: Int) => a + b)
  // val f2 = (a: Int, b: Int) => a + b
  // val f3 = (_: Int) + (_: Int)

  // Using explicit types
  // val f4: (Int, Int) => Int = (_ + _)
  // val f5: (Int, Int) => Int = ((a,b) => a + b)

  // Currying, using type inference
  // val add = ((x:Int) => x + (_:Int))
  // val add = ((x:Int) => (y:Int) => x + y)

  // Currying with explicit types
  // val add: Int => (Int => Int) = (x => x + _)
  // val add: Int => (Int => Int) = (x => y => x + y)

  ////////////////////////////////////
  // Method that returns a function
  ////////////////////////////////////

  // Using type inference
  // def add(a: Int) = (a + (_: Int))
  // def add(a: Int) = ((x:Int) => a + x)

  // Using explicit types
  // def add(a: Int): Int => Int = (a + (_: Int))
  // def add(a: Int): Int => Int = (x => a + x)

  // Method returns double
  //def targetFunction4(x:Double) = x * m + b



