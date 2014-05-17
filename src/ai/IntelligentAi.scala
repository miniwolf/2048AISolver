package ai

import gamelogic._

class IntelligentAi(game: Game) {


  // http://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning
  /*def search(depth: Int, alpha: Int, beta: Int, cPositions: Int, cCutoffs: Int): (Direction, Int, Int, Int) = {
    var positions = cPositions
    var bestScore = 0
    var bestMove = -1
    var result: (Direction, Int) = (Up, 0)

    //The maximizing player
    bestScore = alpha
    for ( direction <- List(Left, Right, Up, Down) ) {
      val newGame = game.clone
      if ( newGame.move(direction) ) {
        positions += 1
        if ( newGame.win )
          return (direction, 10000, positions, cutoffs)

        val newAI = new GreedyAI(newGame)

        if ( depth == 0 )
          result = (direction, newAI.eval)
      }
    }
  }*/
}
