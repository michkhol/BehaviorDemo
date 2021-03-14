package org.example

sealed trait Protocol
case class CloseWindow(id: Int) extends Protocol
case class OpenWindow(id: Int) extends Protocol
case object Ignore extends Protocol

object Example {
  val setup: Behavior[Protocol] = Behaviors.setup { context =>
    active(0, 0)
  }

  def active(oid: Int, cid: Int): Behavior[Protocol] = Behaviors.receive { (ctx, m) =>
    m match {
      case CloseWindow(id) => active(oid, id)
      case OpenWindow(id) => active(id, cid)
      case Ignore => Behaviors.same
    }
  }

  def main(args: String*): Unit = {
    new EventHandler[Protocol](setup) {
      override val context: Context = ???

      override def eventToMessage(ev: Event): Protocol = ???
    }
  }
}
