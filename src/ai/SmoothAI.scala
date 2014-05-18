package ai

import gamelogic._

class SmoothAI extends AI {
  override def eval(manager: GameManager): Int = {
    val smoothWeight: Double = 1.0

    (manager.smoothness * smoothWeight).toInt
  }

  def tempAI: AI = new SmoothAI
}
