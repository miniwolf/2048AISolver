package gamelogic

import java.util
import javax.swing._
import java.awt._
import ai.Mover

object Game {
  private def ensureSize(l: util.List[Tile], s: Int) {
    while ( l.size != s )
      l.add(new Tile)
  }

  private def offsetCoors(arg: Int): Int = arg * (TILES_MARGIN + TILE_SIZE) + TILES_MARGIN

  def main(args: Array[String]) {
    val window: JFrame = new JFrame
    window.setTitle("2048 Game - AI Solver")
    window.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    window.setSize(340, 400)
    window.setResizable(false)
    val game = new Game()
    window.add(game)
    window.setLocationRelativeTo(null)
    window.setVisible(true)

    new GameManager(game).run()
  }

  private final val BG_COLOR: Color = new Color(0xbbada0)
  private final val FONT_NAME: String = "Arial"
  private final val TILE_SIZE: Int = 64
  private final val TILES_MARGIN: Int = 16
}

class Game extends JPanel {
  var tiles: Array[Tile] = Array.ofDim[Tile](16)
  var win: Boolean = false
  var lose: Boolean = false
  var score: Int = 0
  setFocusable(true)
  addKeyListener(new Mover(this))
  resetGame()

  def resetGame() {
    score = 0
    win = false
    lose = false

    tiles = Array.ofDim[Tile](4 * 4)
    (0 until tiles.length).foreach { case i => tiles(i) = new Tile() }

    addTile()
    addTile()

    game.repaint()
  }

  override def clone: Game = {
    val newGame = new Game
    for ( x <- 0 until 4; y <- 0 until 4 )
      newGame.insertTile(tileAt(x, y), x, y)
    newGame
  }

  def insertTile(tile: Tile, x: Int, y: Int) {
    tiles(x + y * 4) = tile
  }

  def move(direction: Direction): Boolean = {
    direction match {
      case Left =>
        var needAddTile: Boolean = false
        for ( i <- 0 until 4 ) {
          val line: Array[Tile] = getLine(i)
          val merged: Array[Tile] = mergeLine(moveLine(line))
          setLine(i, merged)
          if ( !needAddTile && !compare(line, merged) )
            needAddTile = true
        }
        if ( needAddTile ) {
          addTile()
          return true
        }
        false
      case Right =>
        tiles = rotate(180)
        val moved = move(Left)
        tiles = rotate(180)
        moved
      case Up =>
        tiles = rotate(270)
        val moved = move(Left)
        tiles = rotate(90)
        moved
      case Down =>
        tiles = rotate(90)
        val moved = move(Left)
        tiles = rotate(270)
        moved
    }
  }

  private def tileAt(x: Int, y: Int): Tile = tiles(x + y * 4)

  private def addTile() {
    val list: Array[Tile] = availableSpace
    if ( !availableSpace.isEmpty ) {
      val index: Int = (Math.random * list.size).asInstanceOf[Int] % list.size
      val emptyTime: Tile = list(index)
      emptyTime.value = if ( Math.random < 0.9 ) 2 else 4
    }
  }

  def availableSpace: Array[Tile] = tiles.filter(p => p.isEmpty )

  private def isFull: Boolean = availableSpace.size == 0

  def canMove: Boolean = {
    if (!isFull) return true

    for ( x <- 0 until 4; y <- 0 until 4 ) {
        val t: Tile = tileAt(x, y)
        if ( ( x < 3 && t.value == tileAt(x + 1, y).value )
            || ( (y < 3) && t.value == tileAt(x, y + 1).value) )
          return true
    }
    false
  }

  private def compare(line1: Array[Tile], line2: Array[Tile]): Boolean = {
    if ( line1 eq line2 ) return true
    else if ( line1.length != line2.length ) return false

    for ( i <- 0 until line1.length )
      if ( line1(i).value != line2(i).value ) return false
    true
  }

  private def rotate(angle: Int): Array[Tile] = {
    val newTiles: Array[Tile] = Array.ofDim[Tile](4 * 4)
    var offsetX: Int = 3
    var offsetY: Int = 3
    if ( angle == 90 ) offsetY = 0
    else if ( angle == 270 ) offsetX = 0

    val rad: Double = Math.toRadians(angle)
    val cos: Int = Math.cos(rad).asInstanceOf[Int]
    val sin: Int = Math.sin(rad).asInstanceOf[Int]
    for ( x <- 0 until 4; y <- 0 until 4 ) {
      val newX: Int = (x * cos) - (y * sin) + offsetX
      val newY: Int = (x * sin) + (y * cos) + offsetY
      newTiles(newX + newY * 4) = tileAt(x, y)
    }
    newTiles
  }

  private def moveLine(oldLine: Array[Tile]): Array[Tile] = {
    val l: util.LinkedList[Tile] = new util.LinkedList[Tile]
    for ( i <- 0 until 4 ) if ( !oldLine(i).isEmpty )
      l.addLast(oldLine(i))

    if ( l.size == 0 ) {
      oldLine
    } else {
      val newLine: Array[Tile] = Array.ofDim[Tile](4)
      Game.ensureSize(l, 4)
      for ( i <- 0 until 4 )
        newLine(i) = l.removeFirst()
      newLine
    }
  }

  private def mergeLine(oldLine: Array[Tile]): Array[Tile] = {
    val list: util.LinkedList[Tile] = new util.LinkedList[Tile]
    var i: Int = 0
    while ( i < 4 && !oldLine(i).isEmpty ) {
      var num: Int = oldLine(i).value
      if ( i < 3 && oldLine(i).value == oldLine(i + 1).value ) {
        num *= 2
        score += num
        win = num == 2048
        i += 1
      }
      list.add(new Tile(num))
      i += 1
    }
    if ( list.size == 0 ) {
      oldLine
    } else {
      Game.ensureSize(list, 4)
      list.toArray(Array.ofDim[Tile](4))
    }
  }

  private def getLine(index: Int): Array[Tile] = {
    val result: Array[Tile] = Array.ofDim[Tile](4)
    for ( i <- 0 until 4 )
      result(i) = tileAt(i, index)
    result
  }

  private def setLine(index: Int, re: Array[Tile]) {
    System.arraycopy(re, 0, tiles, index * 4, 4)
  }

  override def paint(g: Graphics) {
    super.paint(g)
    g.setColor(Game.BG_COLOR)
    g.fillRect(0, 0, this.getSize().width, this.getSize().height)
    for ( y <- 0 until 4; x <- 0 until 4 )
      drawTile(g, tileAt(x, y), x, y)
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
    if ( win || lose ) {
      g.setColor(new Color(255, 255, 255, 30))
      g.fillRect(0, 0, getWidth, getHeight)
      g.setColor(new Color(78, 139, 202))
      g.setFont(new Font(Game.FONT_NAME, Font.BOLD, 48))
      if ( win )
        g.drawString("You won!", 68, 150)
      if ( lose ) {
        g.drawString("Game over!", 50, 130)
        g.drawString("You lose!", 64, 200)
      }
      if ( win || lose ) {
        g.setFont(new Font(Game.FONT_NAME, Font.PLAIN, 16))
        g.setColor(new Color(128, 128, 128, 128))
        g.drawString("Press ESC to play again", 80, getHeight - 40)
      }
    }
    g.setFont(new Font(Game.FONT_NAME, Font.PLAIN, 18))
    g.drawString("Score: " + score, 200, 365)
  }
}