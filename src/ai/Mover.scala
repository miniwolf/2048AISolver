package ai

import java.awt.event.{KeyAdapter, KeyEvent}
import gamelogic.{Game, Up, Down, Left, Right}

class Mover(game: Game) extends KeyAdapter {
  override def keyPressed(e: KeyEvent) {
    if ( e.getKeyCode == KeyEvent.VK_ESCAPE )
      game.resetGame()

    if ( !game.canMove )
      game.lose = true

    if ( !game.win && !game.lose ) e.getKeyCode match {
      case KeyEvent.VK_LEFT => game.move(Left)
      case KeyEvent.VK_RIGHT => game.move(Right)
      case KeyEvent.VK_UP => game.move(Up)
      case KeyEvent.VK_DOWN => game.move(Down)
      case _ =>
    }

    game.repaint()
  }
}
