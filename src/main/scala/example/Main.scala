package example

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors

object Main {
  val layout: String =
    """***P
      |WGP*
      |G***
      |**P*""".stripMargin

  def main(args: Array[String]): Unit = {
    val environment = new Environment(layout)
    val navigator = new Navigator
    val speleologist = new Speleologist


    val system: ActorSystem[Any] = ActorSystem(Behaviors.setup[Any](context => {
      val envRef = context.spawn(environment.envBehavior, "environment")
      val navRef = context.spawn(navigator.navigatorActor, "navigator")
      val spelRef = context.spawn(speleologist.setupActor(navRef, envRef), "speleologist")
      spelRef ! Speleologist.StartRecieve()
      Behaviors.same
    }), "system")
  }
}