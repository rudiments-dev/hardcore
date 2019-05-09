package dev.rudiments.hardcore.dsl

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

trait HardSkill extends PF1 {

  override def apply(command: Command): Event = {
    Await.result(async(command), Skill.defaultTimeout)
  }

  def sync(command: Command): Event
  def async(command: Command): Future[Event]

  def orElse(t: HardSkill): HardSkill = OrElseSkill(this, t)
}

object NoSkill extends HardSkill {
  override def sync(command: Command) = NoHandler(command)
  override def async(command: Command): Future[Event] = {
    Promise[Event].success(NoHandler(command)).future
  }

  override def isDefinedAt(x: Command): Boolean = true
}

trait Skill extends HardSkill {
  val f: PF1

  override def sync(command: Command): Event = f match {
    case t: HardSkill => t.sync(command)
    case pf: PF1 => pf(command)
  }

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
}

object Skill {
  val defaultTimeout: Duration = Duration("5 seconds")
}


case class OrElseSkill(t1: HardSkill, t2: HardSkill) extends HardSkill {

  override def sync(command: Command): Event = {
    if(t1.isDefinedAt(command)) t1.sync(command)
    else if(t2.isDefinedAt(command)) t2.sync(command)
    else NoHandler(command)
  }

  override def async(command: Command): Future[Event] = {
    if(t1.isDefinedAt(command)) t1.async(command)
    else if(t2.isDefinedAt(command)) t2.async(command)
    else Promise.failed(NoHandler(command)).future
  }

  override def isDefinedAt(x: Command): Boolean = t1.isDefinedAt(x) || t2.isDefinedAt(x)
}

object OrElseSkill {
  def apply(t1: HardSkill, t2: HardSkill): HardSkill = (t1, t2) match {
    case (NoSkill, NoSkill) => NoSkill
    case (a: HardSkill, NoSkill) => a
    case (NoSkill, b: HardSkill) => b
    case (a: HardSkill, b: HardSkill) => new OrElseSkill(a, b)
  }
}


trait DependentSkill extends HardSkill {
  val es: Memory
  val f: PF2
  val r: Resolver

  override def sync(command: Command): Event = apply(command)

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
      Skill.defaultTimeout
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

trait Memory {
  def state(): Future[Seq[Event]]
  def commands(): Future[Map[Command, Event]]
  def state(command: Command): Future[Option[Event]]
  def countEvents(): Future[Long]

  def skill(f: PF1): Skill
  def dependent(r: Resolver)(f: PF2): DependentSkill
}

trait SkillSet extends PF1 {
  override def apply(command: Command): Event = {
    Await.result(async(command), Skill.defaultTimeout)
  }



  def withSkill(g: PF1): SkillSet
  def withDependency(r: Resolver)(g: PF2): SkillSet

  def f: HardSkill
  def sync(command: Command): Event = f.sync(command)
  def async(command: Command): Future[Event] = f.async(command)
  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
}