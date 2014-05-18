package ai

import gamelogic._

class GreedyAI extends AI {
  def eval(manager: GameManager): Int = {
    val available: Int = manager.availableSpace.length
    val emptyWeight: Double = 2.7
    val highScore: Double = 1.0

    (manager.score * highScore +
    available * emptyWeight).toInt
  }

  def tempAI: AI = new GreedyAI
}
