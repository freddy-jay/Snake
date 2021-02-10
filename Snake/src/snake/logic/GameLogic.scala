package snake.logic

import java.nio.file.DirectoryIteratorException

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

import scala.collection.mutable

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims : Dimensions) {

  private var previousGameFrames : scala.collection.mutable.Stack[GameFrame] = mutable.Stack[GameFrame](GameFrame(gridDims,random, applePosition = null, gameOver = false).startingGameFrame())
  var currentGameFrame: GameFrame = previousGameFrames.top
  var reverse: Boolean = false
  def gameOver: Boolean = currentGameFrame.gameOver

  def step(): Unit = {
    if (reverseIsPossible)
      currentGameFrame = previousGameFrames.pop()
    else if (!currentGameFrame.gameOver) {
      previousGameFrames.push(currentGameFrame.moveSnake())
      currentGameFrame = previousGameFrames.top
    }
  }

  def setReverse(r: Boolean): Unit = {
    reverse = r
    if(reverseIsPossible) currentGameFrame = previousGameFrames.pop()
  }

  def changeDir(d: Direction): Unit = {
    currentGameFrame.changeDir(d)
  }

  def getCellType(p : Point): CellType = {
    if (p == currentGameFrame.body.head)                                                    SnakeHead(currentGameFrame.curDir)
    else if (currentGameFrame.isApple(p))                                                   Apple()
    else if (currentGameFrame.body contains p)                                              SnakeBody()
    else                                                                                    Empty()
  }

  def reverseIsPossible : Boolean = {
    if(reverse && previousGameFrames.size >= 0) true
    else                                        false
  }

}

case class GameFrame (
                       gridDims: Dimensions,
                       randomGenerator: RandomGenerator,
                       curDir: Direction = East(),
                       body: List[Point] = List(Point(2, 0), Point(1, 0), Point(0, 0)),
                       snakeLength: Int = 3,
                       applePosition: Point,
                       gameOver: Boolean
                     ) {
  var newDirection: Direction = null

  def changeDir(d: Direction): Unit = {
    if (d != curDir.opposite) newDirection = d
  }

  def isApple(moveTo : Point): Boolean = {
    if(this.applePosition != null) moveTo == applePosition
    else                           false
  }

  def moveSnake(): GameFrame = {
    var direction : Direction = null
    val moveTo = {
      if (newDirection == null) {
        direction = curDir
        body.head + curDir.toPoint
      } else {
        direction = newDirection
        body.head + newDirection.toPoint
      }
    }

    //Exits from East
    if (moveTo.x >= gridDims.width)                       updateSnake(Point(0, moveTo.y), direction)
    //Exits from South
    else if (moveTo.y >= gridDims.height)                 updateSnake(Point(moveTo.x, 0), direction)
    //Exits from west
    else if (moveTo.x < 0)                                updateSnake(Point(gridDims.width - 1, moveTo.y), direction)
    //Exits from North
    else if (moveTo.y < 0)                                updateSnake(Point(moveTo.x, gridDims.height - 1), direction)
    else                                                  updateSnake(moveTo, direction)
  }

  def updateSnake(headPosition: Point, direction: Direction): GameFrame = {
    val tempBody = body.prepended(headPosition).take(snakeLength)

    if(isApple(headPosition))                                   copy(body = tempBody, applePosition = findSpotForApple(tempBody), curDir = direction,snakeLength = snakeLength + 3)
    else if (tempBody.drop(1) contains headPosition)            copy(gameOver = true)
    else                                                        copy(body = body.prepended(headPosition).take(snakeLength), curDir = direction)
  }

  def findSpotForApple(body: List[Point]): Point = {
    var emptySpots: Seq[Point] = Seq()

    for (p <- gridDims.allPointsInside) {
      if (!body.contains(p))
        emptySpots = emptySpots :+ p
    }
    if (emptySpots.isEmpty) null
    else emptySpots(randomGenerator.randomInt(emptySpots.size))
  }

  def startingGameFrame(): GameFrame = {
    copy(
      curDir = East(),
      body = List(Point(2, 0), Point(1, 0), Point(0, 0)),
      snakeLength = 3,
      applePosition = findSpotForApple(body))
  }

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims
    : Dimensions =
    Dimensions(width = 25, height = 25)  // you can adjust these values to play on a different sized board



}


