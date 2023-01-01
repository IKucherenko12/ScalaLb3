package example

import Handler.{ROOM_STATUS_FALSE, ROOM_STATUS_NO_STATUS, ROOM_STATUS_TRUE}
import example.traits.{Forward, Grab, Look, LookDown, LookLeft, LookRight, LookUp, Shoot, SpeleologistAction}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Handler {
  val pathfinder = new Handler

  var ROOM_STATUS_TRUE = 1
  var ROOM_STATUS_FALSE = 2
  var ROOM_STATUS_POSSIBLE = 3
  var ROOM_STATUS_NO_STATUS: Int = -1

  val START = "start"
  val WUMPUS = "wumpus"
  val PIT = "pit"
  val BREEZE = "breeze"
  val STENCH = "stench"
  val SCREAM = "scream"
  val BUMP = "bump"


  def calculateAction(percept: WumpusPercept): (Look, SpeleologistAction) = pathfinder.get_actions(percept)
}

class Handler {
  var imaginatyWorld: ImaginaryWampusWorld = new ImaginaryWampusWorld
  private var agentPosition = Position(0, 0)
  private var agentsWayStory = mutable.ListBuffer[(Int, Int)]()
  private var moveRoom = false
  private var agentX = 0
  private var agentY = 0

  private def getOkNeighbors(agentPosition: Position): List[Position] = {
    val okNeighbors = getNeighborsPosition(agentPosition)
    val okPositions = ListBuffer[Position]()
    for (position <- okNeighbors) {
      if (!imaginatyWorld.worldGrid.contains(position))
        this.imaginatyWorld.worldGrid = this.imaginatyWorld.worldGrid + (position -> new ImaginaryRoom)
      if ((this.imaginatyWorld.worldGrid(position).ok == Handler.ROOM_STATUS_TRUE
        && this.imaginatyWorld.worldGrid(position).noWay != Handler.ROOM_STATUS_TRUE
        && this.imaginatyWorld.worldGrid(position).exist != Handler.ROOM_STATUS_FALSE)
        || this.imaginatyWorld.worldGrid(position).ok == Handler.ROOM_STATUS_NO_STATUS)
        okPositions += position
    }
    if (okPositions.isEmpty) {
      val (x: Int, y: Int) = agentsWayStory.last
      okPositions.addOne(Position(x, y))
      this.imaginatyWorld.worldGrid(agentPosition).noWay = Handler.ROOM_STATUS_TRUE
    }
    okPositions.toList
  }

  private def setNeighborsPriority(okRooms: List[Position]): Int = {
    var maxPr: Int = -1
    var bestRoom: Position = Position(-1, -1)
    for (room <- okRooms) {
      var priority: Int = 0
      if (imaginatyWorld.worldGrid(room).breeze != ROOM_STATUS_TRUE) {
        priority += 1
      }
      if (imaginatyWorld.worldGrid(room).exist != ROOM_STATUS_FALSE) {
        priority += 1
      }
      if (imaginatyWorld.worldGrid(room).exist == ROOM_STATUS_NO_STATUS) {
        priority += 1
      }
      if (imaginatyWorld.worldGrid(room).noWay != ROOM_STATUS_TRUE) {
        priority += 1
      }
      if (imaginatyWorld.worldGrid(room).ok != ROOM_STATUS_FALSE) {
        priority += 1
      }
      if (imaginatyWorld.worldGrid(room).pit != ROOM_STATUS_TRUE) {
        priority += 1
      }
      if (imaginatyWorld.worldGrid(room).stench != ROOM_STATUS_TRUE) {
        priority += 1
      }
      if (imaginatyWorld.worldGrid(room).wampus != ROOM_STATUS_TRUE) {
        priority += 1
      }
      if (priority > maxPr) {
        maxPr = priority
        bestRoom = room
      }

    }
    okRooms.indexOf(bestRoom)
  }

  private def updateNeighbors(agentPosition: Position): Unit = {
    val currentRoom = imaginatyWorld.worldGrid(agentPosition)

    val roomList = getNeighborsImaginaryRoom(agentPosition)
    if (currentRoom.stench == Handler.ROOM_STATUS_TRUE) {
      imaginatyWorld.wampusRoomCount = imaginatyWorld.wampusRoomCount + 1
      for (room <- roomList) {
        if (room.wampus == Handler.ROOM_STATUS_NO_STATUS) {
          room.ok = Handler.ROOM_STATUS_POSSIBLE
          room.wampus = Handler.ROOM_STATUS_POSSIBLE
        }
      }
    }
    if (currentRoom.breeze == Handler.ROOM_STATUS_TRUE) {
      for (room <- roomList) {
        if (room.pit == Handler.ROOM_STATUS_NO_STATUS) {
          room.ok = Handler.ROOM_STATUS_POSSIBLE
          room.pit = Handler.ROOM_STATUS_POSSIBLE
        }
      }
    }

    if (currentRoom.breeze == Handler.ROOM_STATUS_FALSE && currentRoom.stench == Handler.ROOM_STATUS_FALSE) {
      for (room <- roomList) {
        room.ok = Handler.ROOM_STATUS_TRUE
        room.wampus = Handler.ROOM_STATUS_FALSE
        room.pit = Handler.ROOM_STATUS_FALSE
      }
    }
  }

  private def get_actions(wumpusPercept: WumpusPercept) = {
    Console.println("I am in " + agentX + " " + agentY + "")
    var actions: (Look, SpeleologistAction) = null
    var checking_room = imaginatyWorld.worldGrid getOrElse(agentPosition, null)
    if (checking_room == null) {
      checking_room = new ImaginaryRoom
      imaginatyWorld.worldGrid = imaginatyWorld.worldGrid + (agentPosition -> checking_room)
    }
    var room_info = ""
    if (wumpusPercept.bump) {
      room_info += Handler.BUMP + ","
    }
    if (wumpusPercept.breeze) {
      room_info += Handler.BREEZE + ","
    }
    if (wumpusPercept.glitter) {
      actions = (LookUp, Grab)
    }
    if (wumpusPercept.scream) {
      room_info += Handler.SCREAM + ","
    }
    if (wumpusPercept.stench) {
      room_info += Handler.STENCH + ","
    }
    if (room_info.isEmpty) {
      Console.println("nothing found")
    }
    else {
      Console.println("")
    }
    if (actions == null) {
      if (!wumpusPercept.bump) {
        val agentStory = agentsWayStory
        agentStory.addOne((agentPosition.x, agentPosition.y))
        agentPosition.x = agentX
        agentPosition.y = agentY
        if (imaginatyWorld.worldGrid(agentPosition).exist != Handler.ROOM_STATUS_TRUE) {
          imaginatyWorld.worldGrid(agentPosition).exist = Handler.ROOM_STATUS_TRUE
        }
        moveRoom = false
      }
      else {
        val helpPosition = Position(agentX, agentY)
        imaginatyWorld.worldGrid(helpPosition).exist = Handler.ROOM_STATUS_FALSE
        imaginatyWorld.worldGrid(helpPosition).ok = Handler.ROOM_STATUS_FALSE
      }
      checking_room = imaginatyWorld.worldGrid(agentPosition)
      if (checking_room == null) {
        checking_room = new ImaginaryRoom
        imaginatyWorld.worldGrid = imaginatyWorld.worldGrid + (agentPosition -> checking_room)
      }
      if (checking_room.ok != Handler.ROOM_STATUS_TRUE)
        checking_room.ok = Handler.ROOM_STATUS_TRUE
      for (event <- room_info.split(',')) {
        checking_room.addEvent(event)
      }
      updateNeighbors(agentPosition)
      if (imaginatyWorld.isWampusAlive && imaginatyWorld.wampusRoomCount > 2) {
        val wampusPosition = imaginatyWorld.getWampusCoords
        actions = getNextRoomAction(agentPosition, wampusPosition, Shoot)
      }
      else {
        val nextOkRooms = getOkNeighbors(agentPosition)
        var best_candidate = -1
        best_candidate = setNeighborsPriority(nextOkRooms)
        Console.println("Go to: " + nextOkRooms(best_candidate).x + "  " + nextOkRooms(best_candidate).y)
        actions = getNextRoomAction(agentPosition, nextOkRooms(best_candidate), Forward)
        Console.println("and  " + actions._1)
      }
    }

    actions
  }

  private def getNextRoomAction(agentPosition: Position, nextOkRoom: Position, action: SpeleologistAction): (Look, SpeleologistAction) = {
    agentX = agentPosition.x
    agentY = agentPosition.y
    var look: Look = LookUp
    if (agentPosition.y < nextOkRoom.y) {
      agentY += 1
      look = LookRight
    }
    else if (agentPosition.y > nextOkRoom.y) {
      agentY -= 1
      look = LookLeft
    }
    else if (agentPosition.x < nextOkRoom.x) {
      agentX += 1
      look = LookDown
    }
    else {
      agentX -= 1
      look = LookUp
    }
    moveRoom = true
    (look, action)
  }

  private def getNeighborsImaginaryRoom(agentPosition: Position) = {
    val rightNeighbor = Position(agentPosition.x + 1, agentPosition.y)
    val upNeighbor = Position(agentPosition.x, agentPosition.y + 1)
    val leftNeighbor = Position(agentPosition.x - 1, agentPosition.y)
    val bottomNeighbor = Position(agentPosition.x, agentPosition.y - 1)
    var rightRoom = imaginatyWorld.worldGrid getOrElse(rightNeighbor, null)
    if (rightRoom == null) {
      rightRoom = new ImaginaryRoom
      imaginatyWorld.worldGrid = imaginatyWorld.worldGrid + (rightNeighbor -> rightRoom)
    }
    var upRoom = imaginatyWorld.worldGrid getOrElse(upNeighbor, null)
    if (upRoom == null) {
      upRoom = new ImaginaryRoom
      imaginatyWorld.worldGrid = imaginatyWorld.worldGrid + (rightNeighbor -> upRoom)
    }
    var leftRoom = imaginatyWorld.worldGrid getOrElse(leftNeighbor, null)
    if (leftRoom == null) {
      leftRoom = new ImaginaryRoom
      imaginatyWorld.worldGrid = imaginatyWorld.worldGrid + (rightNeighbor -> leftRoom)
    }
    var bottomRoom = imaginatyWorld.worldGrid getOrElse(bottomNeighbor, null)
    if (bottomRoom == null) {
      bottomRoom = new ImaginaryRoom
      imaginatyWorld.worldGrid = imaginatyWorld.worldGrid + (rightNeighbor -> bottomRoom)
    }
    val rooms = Array[ImaginaryRoom](rightRoom, upRoom, leftRoom, bottomRoom)
    rooms
  }

  private def getNeighborsPosition(agentPosition: Position) = {
    val rightNeighbor = Position(agentPosition.x + 1, agentPosition.y)
    val upNeighbor = Position(agentPosition.x, agentPosition.y + 1)
    val leftNeighbor = Position(agentPosition.x - 1, agentPosition.y)
    val bottomNeighbor = Position(agentPosition.x, agentPosition.y - 1)

    Array[Position](rightNeighbor, upNeighbor, leftNeighbor, bottomNeighbor)
  }
}

class ImaginaryWampusWorld() {
  var worldGrid: Map[Position, ImaginaryRoom] = Map[Position, ImaginaryRoom]()
  var isWampusAlive = true
  var wampusRoomCount = 0
  var wampusCoords: Position = _

  def getWampusCoords: Position = {
    var xWampusCoord = 0
    var yWampusCoord = 0
    val keys = worldGrid.keySet

    for (roomPosition <- keys) {
      val room: Option[ImaginaryRoom] = worldGrid.get(roomPosition)
      if (room.nonEmpty && room.get.wampus == Handler.ROOM_STATUS_POSSIBLE) {
        xWampusCoord += roomPosition.x
        yWampusCoord += roomPosition.y
      }
    }
    xWampusCoord /= wampusRoomCount
    yWampusCoord /= wampusRoomCount
    this.wampusCoords = Position(xWampusCoord, yWampusCoord)

    this.wampusCoords
  }
}

class ImaginaryRoom() {
  var exist: Int = Handler.ROOM_STATUS_NO_STATUS
  var stench: Int = Handler.ROOM_STATUS_NO_STATUS
  var breeze: Int = Handler.ROOM_STATUS_NO_STATUS
  var pit: Int = Handler.ROOM_STATUS_NO_STATUS
  var wampus: Int = Handler.ROOM_STATUS_NO_STATUS
  var ok: Int = Handler.ROOM_STATUS_NO_STATUS
  var noWay: Int = Handler.ROOM_STATUS_NO_STATUS

  def addEvent(event_name: String): Unit = {
    event_name match {
      case Handler.START =>

      case Handler.WUMPUS =>
        this.wampus = Handler.ROOM_STATUS_TRUE

      case Handler.PIT =>
        this.pit = Handler.ROOM_STATUS_TRUE

      case Handler.BREEZE =>
        this.breeze = Handler.ROOM_STATUS_TRUE

      case Handler.STENCH =>
        this.stench = Handler.ROOM_STATUS_TRUE

      case Handler.SCREAM =>

      case Handler.BUMP =>
      case "" =>

    }
  }
}

case class Position(var x: Int, var y: Int)
