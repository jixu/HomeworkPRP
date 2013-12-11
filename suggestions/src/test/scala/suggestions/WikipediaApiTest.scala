package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable
import rx.lang.scala.subscriptions.Subscription


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("timeout should work correctly") {
    val times = Observable.interval(300 milliseconds)
    val timeouts = times.timedOut(1)
    val seq = mutable.Buffer[Long]()
    def onNext = { l: Long => seq.append(l) }
    timeouts.subscribe(onNext)
    Thread.sleep(2000)
    assert(seq == Seq(0,1,2))
  }

  test("recovered should work correctly") {
    val obs = Observable { observer: Observer[Int] =>
      observer.onNext(1)
      observer.onNext(2)
      observer.onError(new RuntimeException("error"))
      observer.onNext(3)
      observer.onCompleted()
      observer.onNext(4)
      Subscription()
    }

    def onNext = { i: Try[Int] => i match {
      case Success(s) => println("success: " + s)
      case Failure(e) => println("failure: " + e)
    }}
    def onError = { e: Throwable => println(e) }
    def onComplete = { () => println("Complete") }

    obs.recovered.subscribe(onNext, onError, onComplete)
  }

  test("concatRecovered should work correctly") {
    val obs = Observable(1, 2, 3)
    val obsNew = obs.concatRecovered { num =>
      if (num != 2) Observable(num, num, num) else Observable(new Exception)
    }

    obsNew.subscribe { i =>
      println("test: " + i)
    }
  }
}