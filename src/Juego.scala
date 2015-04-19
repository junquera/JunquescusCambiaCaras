import scala.util.Random

class Juego(dificultadN: Int) {

  val colores = 'A' :: 'N' :: 'R' :: 'V' :: 'M' :: 'G' :: 'B' :: Nil
  val dificultad = 3 :: 5 :: 7 :: Nil
  val rnd = new Random()
  val sizeX = 8
  val sizeY = 8

  def print(tabla: List[List[Char]]) = {
    for (x <- 0 to tabla.size - 1) {
      printf("[ ")
      for (y <- 0 to tabla(x).size - 1) {
        printf(tabla(y)(x) + " ")
      }
      println("]")
    }
  }

  def turno(tabla: List[List[Char]]): List[List[Char]] = {
    val aux = juegaTurno(tabla)

    if (aux.equals(tabla)) {
      return tabla
    } else {
      return turno(aux)
    }
  }

  def juegaTurno(tabla: List[List[Char]]): List[List[Char]] = {
    return compactaYRellena(transpose(borraCoincidencias(transpose(borraCoincidencias(tabla)))))
  }

  def borraCoincidencias(tabla: List[List[Char]]): List[List[Char]] = {
    if (tabla.isEmpty)
      return Nil
    else
      return borraCoincidenciasLista(tabla.head) :: borraCoincidencias(tabla.tail)
  }

  def compactaYRellena(tabla: List[List[Char]]): List[List[Char]] = {
    if (tabla.isEmpty)
      return Nil
    else
      return compactaYRellenaLista(tabla.head) :: compactaYRellena(tabla.tail)
  }

  def compactaYRellenaLista(l: List[Char]): List[Char] = {
    if (l.isEmpty)
      return Nil
    else {
      val aux = compacta(l)
      return creaLista(l.size - aux.size) ::: aux
    }
  }

  // Crear tablero aleatorio
  def creaTableroAleatorio(): Tablero = {
    return new Tablero(creaTabla(sizeX, sizeY));
  }

  def getMod(lista: List[Char], i: Integer, mod: Integer): List[Char] = {
    if (lista.isEmpty)
      return Nil
    else if (i.%(8) == mod)
      return lista.head :: getMod(lista.tail, i.+(1), mod)
    else
      return getMod(lista.tail, i.+(1), mod)
  }

  def transpose(tabla: List[List[Char]]): List[List[Char]] = {
    val list = tabla.flatten
    return List(getMod(list, 0, 0),
      getMod(list, 0, 1),
      getMod(list, 0, 2),
      getMod(list, 0, 3),
      getMod(list, 0, 4),
      getMod(list, 0, 5),
      getMod(list, 0, 6),
      getMod(list, 0, 7))
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
    else if (lista.head == '0')
      return false
    else
      return lista.head.equals(lista.tail.head) && todosIguales(lista.tail)
  }

  def borraYCrea(lista: List[Char]): ResultadoYPuntos = {
    val auxList = borraCoincidenciasLista(lista)
    val borrados = lista.length - auxList.length
    return new ResultadoYPuntos(creaLista(borrados) ::: auxList, borrados)
  }

  def borraCoincidenciasLista(lista: List[Char]): List[Char] = {
    val l1 = borraCoincidenciasAux(lista)
    val l2 = borraCoincidenciasAux(lista.reverse).reverse
    if (cuentaElementos('0', l1) > cuentaElementos('0', l2)) {
      return l1
    } else {
      return l2
    }
  }

  def cuentaElementos(c: Char, l: List[Char]): Integer = {
    if (l == Nil)
      return 0
    else if (l.head == c)
      return 1.+(cuentaElementos(c, l.tail))
    else
      return cuentaElementos(c, l.tail)
  }

  def borraCoincidenciasAux(lista: List[Char]): List[Char] = {
    if (lista.length < 3)
      return lista
    else if (todosIguales(lista))
      return nuevaLista('0', lista.length)
    else
      return lista.head :: borraCoincidenciasLista(lista.tail)
  }

  def compacta(lista: List[Char]): List[Char] = {
    if (lista == Nil)
      return Nil
    else if (lista.head.equals('0'))
      return compacta(lista.tail)
    else
      return lista.head :: compacta(lista.tail)
  }

  def nuevaLista(c: Char, n: Integer): List[Char] = {
    if (n == 0)
      return Nil
    else
      return c :: nuevaLista(c, n.-(1))
  }
}

class ResultadoYPuntos(resultado: List[Char], puntos: Integer) {
  val getResultado = resultado
  val getPuntos = puntos
}