package ai

import gamelogic.GameManager

class MonoAI extends AI {
  def eval(manager: GameManager): Int = {
    val monoWeight = 1.0

    (manager.monotonicity() *monoWeight).toInt
  }

  def tempAI: AI = new MonoAI
}
