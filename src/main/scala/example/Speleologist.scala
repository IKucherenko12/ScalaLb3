package example

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import Speleologist.StartRecieve
import example.traits.{ActionResult, AgentDied, Down, GotGold, KeepGoing, LookDown, LookLeft, LookRight, LookUp, Up}

class Speleologist {
  private var navRef: ActorRef[Navigator.ActionRequest] = _
  private var envRef: ActorRef[Environment.Request] = _
  private var environmentBehaviorRef: ActorRef[Environment.Response] = _
  private var navigatorBehaviorRef: ActorRef[Navigator.ActionResponse] = _
  private var gameState: ActionResult = KeepGoing

  private def navigatorBehavior: Behavior[Navigator.ActionResponse] = Behaviors.receive[Navigator.ActionResponse]((context, message) => {
    if (message.look == LookUp) {
      envRef ! Environment.PerformAction(message.action, Up, environmentBehaviorRef)
    }
    else if (message.look == LookRight) {
      envRef ! Environment.PerformAction(message.action, traits.Right, environmentBehaviorRef)
    }
    else if (message.look == LookLeft) {
      envRef ! Environment.PerformAction(message.action, traits.Left, environmentBehaviorRef)
    }
    else if (message.look == LookDown) {
      envRef ! Environment.PerformAction(message.action, Down, environmentBehaviorRef)
    }

    Behaviors.same
  })

  private def environmentBehavior: Behavior[Environment.Response] = Behaviors.receive[Environment.Response]((context, message) => {
    message match {
      case Environment.EnvironmentResponse(percept) =>
        navRef ! Navigator.ActionRequest(percept, "", navigatorBehaviorRef)

        Behaviors.same

      case Environment.ActionResponse(actionResult: ActionResult) =>
        this.gameState = actionResult

        if (gameState == AgentDied) {
          {
            Console.println("Lose")
            Behaviors.stopped
          }
          if (gameState == GotGold) {
            Console.println("Win")
            Behaviors.stopped
          }

        }

        envRef ! Environment.EnvironmentRequest(environmentBehaviorRef)
        Behaviors.same
    }
  })

  def setupActor(navRef: ActorRef[Navigator.ActionRequest], envRef: ActorRef[Environment.Request]): Behavior[StartRecieve] =
    Behaviors.receive((context, message) => {
      this.navRef = navRef
      this.envRef = envRef

      if (environmentBehaviorRef == null) {
        environmentBehaviorRef = context.spawn(environmentBehavior, "speleologist-behavior")
        navigatorBehaviorRef = context.spawn(navigatorBehavior, "speleologist-navigator")
      }

      envRef ! Environment.EnvironmentRequest(environmentBehaviorRef)
      Behaviors.same
    })
}

object Speleologist {

  case class StartRecieve()

}




