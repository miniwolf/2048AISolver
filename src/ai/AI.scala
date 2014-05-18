package ai

import gamelogic._
import java.util
import scala.util.control.Breaks._

trait AI {
  /**
   * Evaluation function, act like heuristic function.
   * @param manager game state manager
   * @return value for the current game state, used for comparison of other game states.
   */
  def eval(manager: GameManager): Int

  /**
   * Calculates the best direction to move
   * according to the specific algorithm
   */
  def getBest(game: Game, manager: GameManager) : Direction = {
    iterativeDeep(game, manager)
  }

  /**
   * Search algorithm which look through the four directions and find the
   * best move for the heuristic values
   * @param manager game state manager
   * @param depth depth in the search tree
   * @param alpha ultimate minimum the minimizing player can be assured of
   * @param beta ultimate maximum the maximizing player can be assured of
   * @return The direction which the best move has been found and the score of the heuristic
   */
  // http://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning
  def search(manager: GameManager, game: Game, depth: Int, alpha: Int, beta: Int): (Direction, Int) = {
    var bestScore = alpha
    var bestMove: Direction = NoDirection
    var score = alpha
    for ( direction <- List(Left, Right, Up, Down) ) {
      var newState = manager.clone
      newState = game.move(newState, direction)
      if ( newState.moved ) {
        if ( newState.win ) {
          return (direction, 10000)
        }

        val newAI: AI = tempAI
        if ( depth == 0 ) {
          score = newAI.eval(newState)
        } else {
          val (_, b) = newAI.search(newState, game, depth - 1, bestScore, beta)
          score = b
          if ( score > 9900 )
            score -= 1 // win, slightly penalizing higher depth from win
        }

        if ( score > bestScore ) {
          bestScore = score
          bestMove = direction
        }

        if ( bestScore > beta ) {
          return (bestMove, beta)
        }
      }
    }
    (bestMove, bestScore)
  }

  /**
   * Helper method
   */
  def tempAI: AI

  /**
   * Iterate a number of times deeper into the search algorithm.
   * Simple algorithms will most likely not benefit from this
   * @param manager game state manager
   * @return The optimal direction chosen by the algorithm.
   *         #see search
   */
  def iterativeDeep(game: Game, manager: GameManager): Direction = {
    var best: Direction = NoDirection
    breakable {
      for ( depth <- 0 to 6 ) {
        val (move, _) = search(manager, game, depth, -10000, 10000)
        if ( move == NoDirection )
          break()
        else
          best = move
      }
    }

    Thread.sleep(10)
    best
  }
}
