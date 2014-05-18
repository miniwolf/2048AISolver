package ai

import gamelogic.{NoDirection, Direction, Game, GameManager}
import scala.util.control.Breaks._

class SmoothAI(game: Game) extends AI {
  override def eval(manager: GameManager): Int = {
    val available: Int = manager.availableSpace.length
    val emptyWeight: Double = 2.7
    val highScore: Double = 1.0
    val smoothWeight: Double = 0.1


    (manager.smoothness * smoothWeight+
      manager.score * highScore +
      available * emptyWeight).toInt
  }

  def tempAI(game: Game): AI = {
    new SmoothAI(game)
  }

  def iterativeDeep(manager: GameManager): Direction = {
    var best: Direction = null
    breakable {
      for ( depth <- 0 to 6 ) {
        val (move, _) = search(manager, game, depth, -10000, 10000)
        if ( move == NoDirection )
          break()
        else
          best = move
      }
    }

    Thread.sleep(100)
    best
  }
}
