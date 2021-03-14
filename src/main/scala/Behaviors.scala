package org.example


trait Context

object BehaviorTags {
  final val ExtensibleBehavior = 0
  final val SetupBehavior = 1
  final val SameBehavior = 2
  final val UnhandledBehavior = 3
}

abstract class Behavior[T](val tag: Int) {
  final def unsafeCast[U]: Behavior[U] = this.asInstanceOf[Behavior[U]]
}

abstract class ExtensibleBehavior[T] extends Behavior[T](BehaviorTags.ExtensibleBehavior) {
  def receive(context: Context, msg: T): Behavior[T]
}

abstract class SetupBehavior[T] extends Behavior[T](BehaviorTags.SetupBehavior) {
  def apply(ctx: Context): Behavior[T]
}

object Behaviors {

  def setup[T](factory: Context => Behavior[T]): Behavior[T] = {
    new SetupBehavior[T] {
      def apply(ctx: Context): Behavior[T] = factory(ctx)
    }
  }

  def receive[T](onMessage: (Context, T) => Behavior[T]): Behavior[T] = {
    new ExtensibleBehavior[T] {
      override def receive(context: Context, msg: T): Behavior[T] = onMessage(context, msg)
    }
  }

  def same[T]: Behavior[T] = SameBehavior.unsafeCast[T]
  def unhandled[T]: Behavior[T] = UnhandledBehavior.unsafeCast[T]

  private object SameBehavior extends Behavior[Nothing](BehaviorTags.SameBehavior)
  private object UnhandledBehavior extends Behavior[Any](BehaviorTags.UnhandledBehavior)

}

trait Event

abstract class EventHandler[T](setup: Behavior[T]) {
  // Create context
  val context: Context

  // Bootstrap with context
  var behavior: Behavior[T] = setup.asInstanceOf[SetupBehavior[T]](context)

  def eventToMessage(ev: Event): T

  val callBack: Event => Unit = { ev =>
    val m = eventToMessage(ev)
    val newBehavior = behavior.asInstanceOf[ExtensibleBehavior[T]].receive(context, m)
    behavior = newBehavior.tag match {
      case BehaviorTags.ExtensibleBehavior => newBehavior
      case BehaviorTags.SameBehavior => behavior
      case BehaviorTags.UnhandledBehavior =>
        // Process unhandled
        behavior
    }
  }

}