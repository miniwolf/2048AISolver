package ai

import java.awt.event.{KeyAdapter, KeyEvent}
import gamelogic._

class Mover(game: Game) extends KeyAdapter {
  override def keyPressed(e: KeyEvent) {
    if ( e.getKeyCode == KeyEvent.VK_ESCAPE )
      Game.manager.resetGame()

    if ( !Game.manager.canMove )
      Game.manager.lose = true

    if ( !Game.manager.win && !Game.manager.lose ) e.getKeyCode match {
      case KeyEvent.VK_LEFT => game.move(Game.manager, Left)
      case KeyEvent.VK_RIGHT => game.move(Game.manager, Right)
      case KeyEvent.VK_UP => game.move(Game.manager, Up)
      case KeyEvent.VK_DOWN => game.move(Game.manager, Down)
      case _ =>
    }

    game.repaint()
  }
}
