package com.CambiaCarasAPA

import scala.swing._
import scala.swing.event._
import java.awt.{ Color, Graphics2D, BasicStroke }
import java.awt.geom._

class UI(val juego: Juego) extends MainFrame {

  title = "Cambia caras"
  preferredSize = new Dimension(500, 500)

  val newGameButton = Button("New Game") { newGame() }
  val quitButton = Button("Quit") { sys.exit(0) }
  val dificultad = new TextField("Dificultad (1,2 o 3)", 4)
  val buttonLine = new BoxPanel(Orientation.Horizontal) {

    contents += dificultad
    contents += newGameButton
    contents += quitButton
    this.maximumSize = new Dimension(300, 30)
  }

  var tablaPrincipal = List(List('E'))

  var x1 = -1
  var y1 = -1
  var x2 = -1
  var y2 = -1

  contents = new BoxPanel(Orientation.Vertical) {
    contents += buttonLine
  }

  def mover(x: Int, y: Int) = {
    val movimientoSugerido = juego.find_move(tablaPrincipal)
    val sugerencia = "(x,y): (" + movimientoSugerido(0) + "," + movimientoSugerido(1) + ") -> (" + movimientoSugerido(2) + "," + movimientoSugerido(3) + ")"
    if (x < 0 || y < 0) {
      Dialog.showMessage(contents.head, "Movimiento ilegal Prueba " + sugerencia, title = "ERROR")
    } else if (x1 == -1) {
      x1 = x
      y1 = y
    } else {
      x2 = x
      y2 = y
      if (juego.isValid(x1, y1, x2, y2)) {
        mueve(x1, y1, x2, y2)
        x1 = -1
        y1 = -1
        x2 = -1
        y2 = -1
      } else {
        Dialog.showMessage(contents.head, "Movimiento ilegal Prueba " + sugerencia, title = "ERROR")
        x1 = -1
        y1 = -1
        x2 = -1
        y2 = -1
      }
    }

  }

  def mueve(x1: Int, y1: Int, x2: Int, y2: Int) = {
    tablaPrincipal = juego.turno(juego.exchange(tablaPrincipal, x1, y1, x2, y2), false)
    paint(tablaPrincipal)
  }

  def color(c: Char): Color = {

    c match {
      case 'A' =>
        return Color.BLUE
      case 'N' =>
        return Color.ORANGE
      case 'R' =>
        return Color.RED
      case 'V' =>
        return Color.GREEN
      case 'M' =>
        return Color.PINK
      case 'G' =>
        return Color.GRAY
      case 'B' =>
        return Color.WHITE
      case _ =>
        return Color.BLACK
    }
  }

  def paint(tabla: List[List[Char]]) {
    contents = new BoxPanel(Orientation.Vertical) {
      contents += buttonLine

      contents += new BoxPanel(Orientation.Horizontal) {
        border = Swing.EmptyBorder(10, 10, 10, 10)
        for (i <- 0 to 7)
          contents += new BoxPanel(Orientation.Vertical) {
            for (j <- 0 to 7) {
              val b = Button(tabla(i)(j).toString()) { mover(i, j) }
              b.background_=(color(tabla(i)(j)))
              b.maximumSize_=(new scala.swing.Dimension(50, 50))
              contents += b
            }
          }
      }
      contents += new Label("Puntos jugador: " + juego.puntosJugador * 10)
    }
  }

  def newGame() {
    try {
      val dificultad = this.dificultad.text.toInt
      if (dificultad.equals(1) || dificultad.equals(2) || dificultad.equals(3)) {
        val juego = new Juego(2)
        tablaPrincipal = juego.creaTabla(8, 8)
        paint(juego.limpiar(tablaPrincipal))
      }
    } catch {
      case e: Exception => Dialog.showMessage(contents.head, "Dificultad inválida", title = "ERROR")
    }
  }

  def turno() {
    val movimiento = juego.find_move(tablaPrincipal)
    tablaPrincipal = juego.turno(juego.exchange(tablaPrincipal, movimiento(0), movimiento(1), movimiento(2), movimiento(3)), true)
    paint(tablaPrincipal)
  }
}

object CambiaCaras {
  def main(args: Array[String]) {

    val juego = new Juego(2)
    val ui = new UI(juego)
    ui.visible = true

  }
}
