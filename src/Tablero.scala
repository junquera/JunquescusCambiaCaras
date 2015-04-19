class Tablero(tabla: List[List[Char]]) {

  val getTabla = tabla

  //  Estrategia ganadora

  def estrategiaGanadora(): Point = {
    for (x <- tabla) {
      for (y <- x) {
      }
    }
    return new Point(0, 0)
  }

  //  Recorre horizontal

  //  Recorre vertical

  //  Borra coincidencias

 
  def borraHorizontal() {
    for (i <- 0 to tabla.length) {

    }
  }

  def borraVertical() {

  }

  //  Imprimir tablero

  def print() = {
    for (x <- tabla) {
      printf("[ ")
      for (y <- x) {
        printf(y.toString()+" ")
      }
      println("]")
    }
  }
  
  



}
