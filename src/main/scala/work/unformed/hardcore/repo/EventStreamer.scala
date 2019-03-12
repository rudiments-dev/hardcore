package work.unformed.hardcore.repo

import akka.NotUsed
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.scaladsl.{Keep, Sink, Source}
import com.typesafe.scalalogging.LazyLogging
import work.unformed.hardcore.dsl.Event

class EventStreamer(implicit val materializer: Materializer) extends LazyLogging {

  private val (in, publisher) = Source.queue[Event[_]](5, OverflowStrategy.backpressure).toMat(Sink.asPublisher(true))(Keep.both).run()

  def publish(event: Event[_]): Unit = {
    in.offer(event)
  }

  def subscribe(): Source[Event[_], NotUsed] = {
    Source.fromPublisher(publisher)
  }

}
