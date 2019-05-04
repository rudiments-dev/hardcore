package dev.rudiments.hardcore.dsl

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

trait HardTask extends PF1 {
  def future(command: Command): Future[Event]

  override def apply(command: Command): Event = {
    Await.result(future(command), Task.defaultTimeout)
  }

  def run(command: Command): Event

  def orElse(t: HardTask): HardTask = OrElseTask(this, t)
}

object NoTask extends HardTask {
  override def run(command: Command) = NoHandler(command)
  override def future(command: Command): Future[Event] = {
    Promise[Event].success(NoHandler(command)).future
  }

  override def isDefinedAt(x: Command): Boolean = true
}

trait Task extends HardTask {
  val f: PF1

  override def run(command: Command): Event = f match {
    case t: HardTask => t.run(command)
    case pf: PF1 => pf(command)
  }

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
}

object Task {
  val defaultTimeout: Duration = Duration("5 seconds")
}


case class OrElseTask(t1: HardTask, t2: HardTask) extends HardTask {

  override def run(command: Command): Event = {
    t1.applyOrElse(command, t2)
  }

  override def future(command: Command): Future[Event] = {
    if(t1.isDefinedAt(command)) t1.future(command)
    else if(t2.isDefinedAt(command)) t2.future(command)
    else Promise.failed(NoHandler(command)).future
  }

  override def isDefinedAt(x: Command): Boolean = t1.isDefinedAt(x) || t2.isDefinedAt(x)
}

object OrElseTask {
  def apply(t1: HardTask, t2: HardTask): HardTask = (t1, t2) match {
    case (NoTask, NoTask) => NoTask
    case (a: HardTask, NoTask) => a
    case (NoTask, b: HardTask) => b
    case (a: HardTask, b: HardTask) => new OrElseTask(a, b)
  }
}


trait DependentTask extends HardTask {
  val es: EventStore
  val f: PF2
  val r: Resolver

  override def run(command: Command): Event = apply(command)

  override def isDefinedAt(command: Command): Boolean = {
    import scala.concurrent.ExecutionContext.Implicits.global
    Await.result(
      for {
        c <- es.state(command).map {
          case Some(_: NoHandler) => false
          case _ => true
        }
        d <- es.state(r(command)).map {
          case Some(_: NoHandler) => false
          case _ => true
        }
      } yield c && d,
      Task.defaultTimeout
    )
  }
}



trait Action {
  val f: PF1
  val command: Command
  val promise: Promise[Event]
}

trait DependentAction {
  val f: PF2
  val command: Command
  val dependency: Command
  val promise: Promise[Event]
}

trait EventStore {
  def state(): Future[Seq[Event]]
  def commands(): Future[Map[Command, Event]]
  def state(command: Command): Future[Option[Event]]
  def countEvents(): Future[Long]


  def task(f: PF1): Task
  def future(f: PF1, command: Command): Future[Event]

  def dependent(r: Resolver)(f: PF2): DependentTask
  def future(f: PF2, command: Command, dependency: Command): Future[Event]

  def complete(command: Command, result: Event): Unit
}

trait TaskStore extends HardTask {
  def f: HardTask

  def withTask(g: PF1): TaskStore
  def withDependency(r: Resolver)(g: PF2): TaskStore

  override def run(command: Command): Event = f.run(command)

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
}