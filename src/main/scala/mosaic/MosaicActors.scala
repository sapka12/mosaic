package mosaic

import akka.actor.Actor
import akka.actor.Actor.Receive

object Orchestrator {
  sealed trait Message

  case class DoMosaic(inputImagePath: String,
                      tileSize: Int,
                      tileFolderPath: String,
                      outputImagePath: String)
      extends Message
}
class Orchestrator(tileWorkers: Int) extends Actor {

  override def receive: Receive = ???
}

class TileWorker extends Actor {
  override def receive: Receive = ???
}
