// Empezar con http://docs.scala-lang.org/overviews/
object HelloWorld {

  def main(args: Array[String]) {

    val juego = new Juego(2)
    val t = juego.creaTabla(juego.sizeX.asInstanceOf[Integer], juego.sizeY.asInstanceOf[Integer])

    
    val x = List('A', 'A','A','B','A','A','A')
  }

}