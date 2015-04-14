import scala.util.Random

class Juego(dificultadN: Int) {

  val colores = 'A' :: 'N' :: 'R' :: 'V' :: 'M' :: 'G' :: 'B' :: Nil
  val dificultad = 3 :: 5 :: 7 :: Nil
  val rnd = new Random()
  val sizeX = 8
  val sizeY = 8

  // Crear tablero aleatorio

  def creaTableroAleatorio(): Tablero = {
    return new Tablero(creaTabla(sizeX, sizeY));
  }

  // Crear tabla aleatorio con n listas

  def creaTabla(n: Integer, y: Integer): List[List[Char]] = {
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

  def shorter(l1: List[Char], l2: List[Char]): List[Char] = {
    if (l1.length < l2.length)
      return l1
    else
      return l2
  }

  def todosIguales(lista: List[Char]): Boolean = {
    if (lista.isEmpty || lista.tail.isEmpty)
      return true
    else
      return lista.head.equals(lista.tail.head) && todosIguales(lista.tail)
  }

  def borraYCrea(lista: List[Char]): ResultadoYPuntos = {
    val aux = borraCoincidencias(lista)
    return new ResultadoYPuntos(creaLista(lista.length - aux.length) ::: aux, aux.length)
  }

  def borraCoincidencias(lista: List[Char]): List[Char] = {
    return shorter(borraCoincidenciasAux(lista), borraCoincidenciasAux(lista.reverse).reverse)
  }

  def borraCoincidenciasAux(lista: List[Char]): List[Char] = {
    if (lista.length < 3)
      return lista
    else if (todosIguales(lista))
      return Nil
    else
      return lista.head :: borraCoincidencias(lista.tail)
  }
}

class ResultadoYPuntos(resultado: List[Char], puntos: Integer) {
  val getResultado = resultado
  val getPuntos = puntos
}