package gamelogic

import ai.{GreedyAI, AI}

class GameManager(game: Game) {
  private val running: Boolean = true
  private val over, won: Boolean = false
  var ai: AI = new GreedyAI(game)

  def run() {
    while ( running && !over && !won ) {
      game.move(ai.getBest._1)
      game.repaint()
    }
  }
}
