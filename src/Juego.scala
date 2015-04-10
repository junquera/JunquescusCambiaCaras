import scala.util.Random

class Juego(dificultadN: Int) {

  val colores = 'A' :: 'N' :: 'R' :: 'V' :: 'M' :: 'G' :: 'B' :: List()
  val dificultad = 3 :: 5 :: 7 :: Nil
  val rnd = new Random()

  val sizeX = 8
  val sizeY = 8

  def creaTableroAleatorio() : Tablero = {
    return new Tablero(creaTabla(sizeX, sizeY));  
  }
  
  // Crear tabla aleatorio con n listas

  def creaTabla(n:Integer, y: Integer): List[List[Char]] = {
    if (n == 0)
      return Nil;
    else
      return creaLista(y) :: creaTabla(n.-(1), y);
  }

  // Crear lista aleatoria n elementos

  def creaLista(n: Integer): List[Char] = {
    if (n == 0)
      return Nil;
    else
      return colores(rnd.nextInt(dificultad(dificultadN))) :: creaLista(n.-(1));
  }
  

  // Imprimir tablero

  //  Estrategia ganadora

  //  Recorre horizontal

  //  Recorre vertical

}