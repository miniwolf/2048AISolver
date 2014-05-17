package ai

import util.control.Breaks._
import gamelogic._
import java.util.Random

class GreedyAI(cGame: Game) extends AI {
  game = cGame

  override def eval: Int = {
    val available: Int = game.availableSpace.length
    val emptyWeight: Double = 1.0

    (available * emptyWeight).toInt
  }

  override def getBest: (Direction, Int, Int) = {
    iterativeDeep
  }

  // http://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning
  def search(depth: Int, alpha: Int, beta: Int, cCutoffs: Int): (Direction, Int, Int) = {
    var cutoffs = cCutoffs
    var bestScore = alpha
    var bestMove: Direction = NoDirection
    var move: Direction = NoDirection
    var score = alpha
    var cutoff = 0

    for ( direction <- List(Left, Right, Up, Down) ) {
      val newGame = game.clone
      if ( newGame.move(direction) ) {
        if ( newGame.win )
          return (direction, 10000, cutoffs)

        val newAI = new GreedyAI(newGame)

        if ( depth == 0 ) {
          move = direction; score = newAI.eval
        } else {
          val (a, b, d) = newAI.search(depth - 1, bestScore, beta, cutoffs)
          move = a; score = b; cutoff = d
          if ( score > 9900 )
            score -= 1 // win, slightly penalizing higher depth from win
          cutoffs = cutoff
        }

        if ( score > bestScore ) {
          bestScore = score
          bestMove = direction
        }

        if ( bestScore > beta ) {
          cutoffs += 1
          return (bestMove, beta, cutoffs)
        }
      }
    }

    if ( bestMove == NoDirection ) {
      bestMove = List(Right, Up, Down)(new Random().nextInt(3))
    }
    (bestMove, bestScore, cutoffs)
  }

  def iterativeDeep: (Direction, Int, Int) = {
    var best: (Direction, Int, Int) = (null, 0, 0)
    breakable {
    for (depth <- 0 to 4) {
      val (move, score, cutoffs) = search(depth, -10000, 10000, 0)
      if ( move == NoDirection ) {
        break()
      } else {
        best = (move, score, cutoffs)
      }
    }
    }
    Thread.sleep(100)
    best
  }
}
