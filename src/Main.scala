// Empezar con http://docs.scala-lang.org/overviews/
object HelloWorld {

  def main(args: Array[String]) {

    val juego = new Juego(2)
    val t = juego.creaTableroAleatorio()
    t.print()

    for (x <- juego.borraYCrea(List('X', 'A', 'A', 'A', 'B')).getResultado) {
      printf(x.toString())
    }

  }

}