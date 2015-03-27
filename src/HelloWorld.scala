// Empezar con http://docs.scala-lang.org/overviews/
object HelloWorld {
  def main(args: Array[String]) {

    val x = "Hola mundo"

    println("Hello, world!" + x)

    val mainList = List(List(1,2,3), 2, 1)
    val with4 = 4 :: mainList // re-uses mainList, costs one :: instance
    val with42 = 42 :: mainList // also re-uses mainList, cost one :: instance
    val shorter = mainList(0).asInstanceOf[List[Int]](0)
    println(shorter)
    
    val tablero = new Tablero()
    
  }
}