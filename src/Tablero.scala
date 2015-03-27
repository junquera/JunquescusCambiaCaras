import Array._
import scala.util.Random

class Tablero (dificultadN: Int) {
  val colores = 'A' :: 'N' :: 'R' :: 'V' :: 'M' :: 'G' :: 'B' :: List()
  val dificultad = 3 :: 5 :: 7 :: List()
  val rnd = new Random()
  
  val sizeX = 8
  val sizeY = 8
  
  var T = ofDim[Char](sizeX, sizeY)
  
  
  for (i <- 0 to sizeX - 1) {
    for (j <- 0 to sizeY - 1) {
      T(i)(j) = colores(rnd.nextInt(dificultad(dificultadN)));
    }
  }
  
  def print(){
    for(i <- 0 to sizeX - 1){
      printf("[")
      for(j <- 0 to sizeY - 1){
        printf(T(i)(j).toString())
      }
      println("]")
    }
  }
}