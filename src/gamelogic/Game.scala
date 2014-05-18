package gamelogic

import javax.swing._
import java.awt._
import ai.{SmoothAI, GreedyAI, AI, Mover}

object Game {
  private def offsetCoors(arg: Int): Int = arg * (TILES_MARGIN + TILE_SIZE) + TILES_MARGIN

  def main(args: Array[String]) {
    val window: JFrame = new JFrame
    window.setTitle("2048 Game - AI Solver")
    window.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    window.setSize(340, 400)
    window.setResizable(false)
    val game = new Game()
    game.addKeyListener(new Mover(game))
    window.add(game)
    window.setLocationRelativeTo(null)
    window.setVisible(true)

    manager = new GameManager(game)
    manager.resetGame()
    ai = new SmoothAI(game)

    while ( running ) {
      manager = game.move(manager, ai.getBest(manager))
      game.repaint()
    }
  }

  private val running = true
  private var ai: AI = null
  private final val BG_COLOR: Color = new Color(0xbbada0)
  private final val FONT_NAME: String = "Arial"
  private final val TILE_SIZE: Int = 64
  private final val TILES_MARGIN: Int = 16

  var manager: GameManager = null
}

class Game extends JPanel {
  setFocusable(true)

  def move(manager: GameManager, direction: Direction): GameManager = {
    if ( manager.availableSpace.length == 0 && !manager.canMove ) { manager.lose = true; return manager }
    var newManager = manager
    direction match {
      case Left =>
        var needAddTile: Boolean = false
        for ( i <- 0 until 4 ) {
          val line: Array[Tile] = newManager.getLine(i)
          val merged: Array[Tile] = newManager.mergeLine(newManager.moveLine(line))
          newManager.setLine(i, merged)
          if ( !needAddTile && !newManager.compare(line, merged) )
            needAddTile = true
        }
        if ( needAddTile ) {
          newManager.addTile()
          newManager.moved = true
        } else {
          newManager.moved = false
        }
      case Right =>
        newManager.tiles = newManager.rotate(180)
        newManager = move(newManager, Left)
        newManager.tiles = newManager.rotate(180)
      case Up =>
        newManager.tiles = newManager.rotate(270)
        newManager = move(newManager, Left)
        newManager.tiles = newManager.rotate(90)
      case Down =>
        newManager.tiles = newManager.rotate(90)
        newManager = move(newManager, Left)
        newManager.tiles = newManager.rotate(270)
      case _ =>
    }
    newManager
  }

  override def paint(g: Graphics) {
    super.paint(g)
    g.setColor(Game.BG_COLOR)
    g.fillRect(0, 0, this.getSize().width, this.getSize().height)
    for ( y <- 0 until 4; x <- 0 until 4 )
      drawTile(g, Game.manager.tileAt(x, y), x, y)
  }

  private def drawTile(g2: Graphics, tile: Tile, x: Int, y: Int) {
    val g: Graphics2D = g2.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE)
    val value: Int = tile.value
    val xOffset: Int = Game.offsetCoors(x)
    val yOffset: Int = Game.offsetCoors(y)
    g.setColor(tile.getBackground)
    g.fillRoundRect(xOffset, yOffset, Game.TILE_SIZE, Game.TILE_SIZE, 14, 14)
    g.setColor(tile.getForeground)
    val size: Int = if ( value < 100 ) 36 else if ( value < 1000 ) 32 else 24
    val font: Font = new Font(Game.FONT_NAME, Font.BOLD, size)
    g.setFont(font)
    val s: String = String.valueOf(value)
    val fm: FontMetrics = getFontMetrics(font)
    val w: Int = fm.stringWidth(s)
    val h: Int = -fm.getLineMetrics(s, g).getBaselineOffsets()(2).asInstanceOf[Int]
    if ( value != 0 ) g.drawString(s, xOffset + (Game.TILE_SIZE - w) / 2, yOffset + Game.TILE_SIZE - (Game.TILE_SIZE - h) / 2 - 2)
    if ( Game.manager.win || Game.manager.lose ) {
      g.setColor(new Color(255, 255, 255, 30))
      g.fillRect(0, 0, getWidth, getHeight)
      g.setColor(new Color(78, 139, 202))
      g.setFont(new Font(Game.FONT_NAME, Font.BOLD, 48))
      if ( Game.manager.win )
        g.drawString("You won!", 68, 150)
      if ( Game.manager.lose ) {
        g.drawString("Game over!", 50, 130)
        g.drawString("You lose!", 64, 200)
      }
      if ( Game.manager.win || Game.manager.lose ) {
        g.setFont(new Font(Game.FONT_NAME, Font.PLAIN, 16))
        g.setColor(new Color(128, 128, 128, 128))
        g.drawString("Press ESC to play again", 80, getHeight - 40)
      }
    }
    g.setFont(new Font(Game.FONT_NAME, Font.PLAIN, 18))
    g.drawString("Score: " + Game.manager.score, 200, 365)
  }
}