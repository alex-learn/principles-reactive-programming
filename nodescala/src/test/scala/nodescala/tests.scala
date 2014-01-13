package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  case class FutureException(msg: String) extends Exception(msg)
  
  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  
  test("Future - all"){
    
    val futures : List[Future[Int]] = List(future(1),future(2),future(3))
    
    val f1: Future[List[Int]] = Future.all(futures :+ Future.failed(FutureException("ha! this failed!")))
    
    f1 onComplete{
      case Success(_) => assert(false)
      case Failure(e) => assert(e==FutureException("ha! this failed!"))
    }
    
    val f2 = Future.all(futures)
    
    f2 onComplete{
      case Success(list) => assert(list==List(1,2,3))
      case Failure(_) => assert(false)
    }
    
  }
  
  test("Future - any"){
    val f1 = List(future(1),Future.never)
    
    Future.any(f1) onComplete{
      case Success(s) => assert(s==1)
      case Failure(f) => assert(false)
    }
    
    val f2 = List(Future.never,Future.failed(FutureException("Fail!")),Future.never)
    
    Future.any(f2) onComplete{
      case Success(s) => assert(false)
      case Failure(f) => assert(f==FutureException("Fail!"))
    }
  }
  
  test("Future - now"){
    assert(Future.always(1).now==1)
    try{
      future{
        Thread.sleep(100)
        1
      } now;
      assert(false)
    } catch {
      case e: NoSuchElementException => //ok
      case _: Throwable => assert(false)
    }
  }
  
  test("Future - continueWith"){
    val f1 = future(1)
    
    val f2 = f1.continueWith(_.isCompleted)
    
    f2 onSuccess{
      case s => assert(s==true)
    }
    
    val f3 = Future.never
    
    val f4 = f3.continueWith(_.isCompleted)
    
    try{
      Await result (f4,100 millis)
      assert(false)
    } catch{
      case e: TimeoutException => //ok
      case _: Throwable => assert(false)
    }
    
  }
  
  test("Future - continue"){
    val f1 = future(1)
    
    val f2 = f1.continue{
      case Success(s) => true
      case Failure(f) => false
    }
    
    assert(Await.result(f2,100 millis)==true)
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




