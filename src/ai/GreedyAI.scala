package ai

import util.control.Breaks._
import gamelogic._
import java.util.Random

class GreedyAI(game: Game) extends AI {
  override def eval(manager: GameManager): Int = {
    val available: Int = manager.availableSpace.length
    val emptyWeight: Double = 2.7
    val highScore: Double = 1.0

    (manager.score * highScore +
    available * emptyWeight).toInt
  }

  def tempAI(game: Game): AI = {
    new GreedyAI(game)
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
