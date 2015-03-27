import Array._

class Tablero {
  val colores = List('R')
  
  var T = ofDim[Char](8, 8)
  for (i <- 0 to 2) {
    for (j <- 0 to 2) {
      T(i)(j) = colores(0);
    }
  }
}