package example

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import Navigator.ActionRequest
import example.traits.{Look, SpeleologistAction}

class Navigator {
  def navigatorActor: Behavior[ActionRequest] = Behaviors.receive((context, message) => {
    val percept = message.wumpusPercept
    val action = Handler.calculateAction(percept)

    message.sender ! Navigator.ActionResponse(action._2, action._1)
    Behaviors.same
  })
}


object Navigator {

  case class ActionRequest(wumpusPercept: WumpusPercept, message: String, sender: ActorRef[ActionResponse])

  case class ActionResponse(action: SpeleologistAction, look: Look)


}
