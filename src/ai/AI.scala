package ai

import gamelogic.{Game, Direction}

trait AI {
  var game: Game = null

  def eval: Int = {
    val available = game.availableSpace.length

    val smoothWeight = 0.1
    val monoWeight = 1.0
    val emptyWeight = 2.7
    val maxWeight = 1.0

    //game.smoothness * smoothWeight +
    //game.monotonicity * monoWeight +
    //Math.log(available) * emptyWeight
    //game.maxValue * maxWeight
    0
  }

  /**
   * Calculates the best direction to move
   * according to the specific algorithm
   */
  def getBest: (Direction, Int, Int)
}
