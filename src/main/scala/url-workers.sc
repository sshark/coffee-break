/**
 *
 *
 * @author Lim, Teck Hooi
 *
 */

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source}

val urls = List[String]("http://www.ibm.com/us/en",
  "https://sg.yahoo.com/",
  "https://www.youtube.com/"
)

case class WaitTime(val period : Long, val url : String)

def timing[U](f : => U, url : String = "") = {
  val start = System.currentTimeMillis()
  f
  WaitTime(System.currentTimeMillis() - start, url)
}

def runParallel() = {
  val f = Future.sequence(urls.map(url => Future {timing(Source.fromURL(url), url)}))
  f.onSuccess{case x => x.foreach(t => println(t.url + " took " + t.period + "ms"))}

  Await.ready(f, Duration.Inf)
}

println("Parallel -> " + timing(runParallel()).period + "ms")
println("Sequentially -> " + timing(urls.map(url => timing(Source.fromURL(url), url)).foreach(t => println(t.url + " took " + t.period + " ms"))).period + "ms")
