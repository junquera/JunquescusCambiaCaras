// Empezar con http://docs.scala-lang.org/overviews/
object HelloWorld {

  def main(args: Array[String]) {

    val juego = new Juego(0)
    val t = juego.creaTableroAleatorio()
    
    juego.turno(t.getTabla)
    
  }

}