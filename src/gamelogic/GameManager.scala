package gamelogic

import java.util

object GameManager {
  private def ensureSize(l: util.List[Tile], s: Int) {
    while ( l.size != s )
      l.add(new Tile)
  }
}

class GameManager(game: Game) {
  val running: Boolean = true
  var lose, win: Boolean = false
  var score: Int = 0
  var moved: Boolean = false

  var tiles: Array[Tile] = Array.ofDim[Tile](16)

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

  override def clone: GameManager = {
    val newState = new GameManager(game)
    for ( x <- 0 until 4; y <- 0 until 4 )
      newState.insertTile(tileAt(x, y), x, y)
    newState.win = win; newState.lose = lose; newState.score = score
    newState
  }

  def smoothness = {
    var smoothness = 0.0
    for ( x <- 0 until 4; y <- 0 until 4 ) if ( !tileAt(x, y).isEmpty ) {
      val value = tileAt(x, y).value
      for ( direction <- List[Direction](Left, Right, Up, Down) ) {
        val targetValue = direction match {
          case Left =>  if ( x - 1 >= 0 ) tileAt(x - 1, y).value else 0.0
          case Right => if ( x + 1 < 4 ) tileAt(x + 1, y).value else 0.0
          case Up =>    if ( y - 1 >= 0 ) tileAt(x, y - 1).value else 0.0
          case Down =>  if ( y + 1 < 4 ) tileAt(x, y + 1).value else 0.0
        }
        if ( targetValue != -Float.NegativeInfinity )
          smoothness -= Math.abs(value - targetValue)
      }
    }
    smoothness
  }

  def insertTile(tile: Tile, x: Int, y: Int) {
    tiles(x + y * 4) = tile
  }

  def tileAt(x: Int, y: Int): Tile = tiles(x + y * 4)

  def isFull: Boolean = availableSpace.size == 0

  def availableSpace: Array[Tile] = tiles.filter(p => p.isEmpty )

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

  def addTile() {
    val list: Array[Tile] = availableSpace
    if ( !availableSpace.isEmpty ) {
      val index: Int = (Math.random * list.size).asInstanceOf[Int] % list.size
      val emptyTime: Tile = list(index)
      emptyTime.value = if ( Math.random < 0.9 ) 2 else 4
    }
  }

  def compare(line1: Array[Tile], line2: Array[Tile]): Boolean = {
    if ( line1 eq line2 ) return true
    else if ( line1.length != line2.length ) return false

    for ( i <- 0 until line1.length )
      if ( line1(i).value != line2(i).value ) return false
    true
  }

  def rotate(angle: Int): Array[Tile] = {
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

  def moveLine(oldLine: Array[Tile]): Array[Tile] = {
    val l: util.LinkedList[Tile] = new util.LinkedList[Tile]
    for ( i <- 0 until 4 ) if ( !oldLine(i).isEmpty )
      l.addLast(oldLine(i))

    if ( l.size == 0 ) {
      oldLine
    } else {
      val newLine: Array[Tile] = Array.ofDim[Tile](4)
      GameManager.ensureSize(l, 4)
      for ( i <- 0 until 4 )
        newLine(i) = l.removeFirst()
      newLine
    }
  }

  def mergeLine(oldLine: Array[Tile]): Array[Tile] = {
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
      GameManager.ensureSize(list, 4)
      list.toArray(Array.ofDim[Tile](4))
    }
  }

  def getLine(index: Int): Array[Tile] = {
    val result: Array[Tile] = Array.ofDim[Tile](4)
    for ( i <- 0 until 4 )
      result(i) = tileAt(i, index)
    result
  }

  def setLine(index: Int, re: Array[Tile]) {
    System.arraycopy(re, 0, tiles, index * 4, 4)
  }
}
