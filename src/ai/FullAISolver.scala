package ai

import gamelogic.GameManager

class FullAISolver extends AI {
  def eval(manager: GameManager): Int = {
    val monoWeight = 1.0
    val smoothWeight: Double = 0.1
    var available: Double = Math.log(manager.availableSpace.length)
    val emptyWeight: Double = 2.7
    val maxWeight: Double = 1.0

    if ( available == Float.NegativeInfinity ) { available = 0 }

    (manager.maxValue * maxWeight +
     available * emptyWeight +
     manager.smoothness * smoothWeight +
     manager.monotonicity() * monoWeight).toInt
  }

  def tempAI: AI = new FullAISolver
}
