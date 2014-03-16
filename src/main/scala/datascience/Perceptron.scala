package datascience

import util.Random._
import scala.None
import scala.xml.dtd.SystemID
import scala.collection.concurrent.RDCSS_Descriptor

object Perceptron {

  val maxHistoricalDataPoints = 10
  var w: (Double, Double, Double) = (0,0,0)

  implicit class TuppleMulti[A: Numeric](t: (A, A, A)) {
    import Numeric.Implicits._
    def * (p: (A, A, A)) = {
      val v1 = p._1 * t._1
      val v2 = p._2 * t._2
      val v3 = p._3 * t._3
      v1 + v2 + v3
    }
    def + (p: (A, A, A)) = {
      val v1 = p._1 + t._1
      val v2 = p._2 + t._2
      val v3 = p._3 + t._3
      (v1, v2, v3)
    }
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
    def randomSign: Int = Seq(-1,1)(nextInt(2))
    def randomDouble = randomSign * nextDouble()
    (randomDouble, randomDouble)
  }

  val randomPoint1 = (-1, -1)//randomPoint()
  val randomPoint2 = (1,1)//randomPoint()

  // In each run, choose a random line in the plane as your target function f (do this by taking two random, uniformly distributed points
  // in [1;-1] x [-1;1] and taking the line passing through them), where one side of the line maps to +1 and the other maps  to  1.
  val targetFunctionF = (x:Double) => {
    val x1 = randomPoint1._1
    val y1 = randomPoint1._2
    val x2 = randomPoint2._1
    val y2 = randomPoint2._2
    val m = (y2 - y1) / (x2 - x1)
    val b = y1 - m * x1
    x * m + b
  }
  val classificationFunction = (point:(Double,Double)) => math.signum( point._2 - targetFunctionF(point._1) )

  val randomTrainingPoints: Int => Seq[(Double,Double)] = (n: Int) => Stream.continually(randomPoint()).take(n).toSeq
  val historicalDataFunction = (n: Int) => randomTrainingPoints(n).map( (x:(Double,Double)) => (x,classificationFunction(x)) )
  val historicalData = historicalDataFunction(maxHistoricalDataPoints)

  val missClassifiedPoints = (hd: Seq[((Double, Double), Double)]) => (w:(Double, Double, Double)) => hd.filter{ x =>
    val point = x._1
    val historicalClassification = x._2
    val a = point._1
    val b = point._2
    val classification = math.signum( (1.0, a, b) * w )
    historicalClassification != classification
  }

  val randomlyPickMissClassifiedPoint: Seq[((Double, Double), Double)] => Option[((Double, Double), Double)] = missClassifiedPoints => {
    val randomIndex = if (missClassifiedPoints.isEmpty) 1 else nextInt(missClassifiedPoints.size)
    if (missClassifiedPoints.isDefinedAt(randomIndex)) Some(missClassifiedPoints(randomIndex)) else None
  }


  def start = {

    var continue: Boolean = true
    val testAgainstHistoricalData = missClassifiedPoints(historicalData)

    val loopCount = loopWhile(continue) {

      val mcps = testAgainstHistoricalData(w)
      val missClassifiedPoint = randomlyPickMissClassifiedPoint(mcps)

      missClassifiedPoint match {
        case Some(x) => val point = x._1; val classification = x._2; w = w + ((1.0, point._1, point._2) * classification)
        case None => continue = false
      }

    }

    println(loopCount)
    println(w)
    println(historicalData)

    historicalData.foreach{x =>
      val point = x._1
      val s = (1.0, point._1:Double, point._2: Double) * w
      val sign = math.signum(s)

      assert(x._2 == sign)
    }

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



