package com.CambiaCarasAPA

import scala.util.Random
import java.util.Scanner;

class Juego(dificultadN: Int) {

  val rnd = new Random()
  val colores = 'A' :: 'N' :: 'R' :: 'V' :: 'M' :: 'G' :: 'B' :: Nil
  val dificultad = 3 :: 5 :: 7 :: Nil
  var puntosJugador = 0
  var puntosMaquina = 0

  def parsePoint(puntos: String): List[Integer] = {
    return List(puntos(1).toInt - 48, puntos(3).toInt - 48, puntos(6).toInt - 48, puntos(8).toInt - 48)
  }


  /**
   * Método que se ejecutará cada turno y llamará a los métodos de juego. Se llama a
   * sí mismo de forma recursiva hasta que, tras la jugada, no hay fichas que eliminar
   */
  def turno(tabla: List[List[Char]], maquina: Boolean): List[List[Char]] = {
    val aux = juegaTurno(tabla, maquina)
    if (aux.equals(tabla))
      return tabla
    else
      return turno(aux, maquina)
  }

  /**
   *  Método de juego. Analiza la tabla, marca con 0 las fichas que hay que eliminar, añade puntos
   *  al juego y elimina los elementos, rellenando los huecos que queden con colores aleatorios
   */
  def juegaTurno(tabla: List[List[Char]], maquina: Boolean): List[List[Char]] = {
    val tablaAnalizada = traspose(borraCoincidencias(traspose(borraCoincidencias(tabla))))
    if (maquina)
      this.puntosMaquina = puntosMaquina + (cuentaElementos('0', tablaAnalizada))
    else
      this.puntosJugador = puntosJugador + (cuentaElementos('0', tablaAnalizada))
    return compactaYRellena(traspose(borraCoincidencias(traspose(borraCoincidencias(tabla)))))
  }

  def limpiar(tabla: List[List[Char]]): List[List[Char]] = {
    val aux = compactaYRellena(traspose(borraCoincidencias(traspose(borraCoincidencias(tabla)))))
    if (aux.equals(tabla)) return tabla
    else return limpiar(aux)
  }

  /**
   * Cuenta el número de ocurrencias de c en el tablero t
   */
  def cuentaElementos(c: Char, tabla: List[List[Char]]): Integer = {
    if (tabla.isEmpty)
      return 0
    else
      return cuentaElementosLista(c, tabla.head).+(cuentaElementos(c, tabla.tail))
  }

  /**
   * Marca como 0 los elementos que se repiten contiguamente 3 o más veces en una tabla.
   */
  def borraCoincidencias(tabla: List[List[Char]]): List[List[Char]] = {
    if (tabla.isEmpty)
      return Nil
    else
      return borraCoincidenciasLista(tabla.head) :: borraCoincidencias(tabla.tail)
  }

  /**
   * Imprime el contenido de una tabla de juego
   */
  def printTabla(tabla: List[List[Char]]) = {
    for (x <- tabla) {
      printf("[ ")
      for (y <- x) {
        printf(y.toString() + " ")
      }
      println("]")
    }
  }

  /**
   * Elimina los ceros y rellena los huecos que quedan tras compactar con colores aleatorios
   */
  def compactaYRellena(tabla: List[List[Char]]): List[List[Char]] = {
    if (tabla.isEmpty)
      return Nil
    else
      return compactaYRellenaLista(tabla.head) :: compactaYRellena(tabla.tail)
  }

  /********************* <TRANSPUESTA> **************/
  /**
   * Métodos para hacer la transpuesta de una tabla
   */

  def traspose(tabla: List[List[Char]]): List[List[Char]] = {
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

  def getMod(lista: List[Char], i: Integer, mod: Integer): List[Char] = {
    if (lista.isEmpty)
      return Nil
    else if (i.%(8) == mod)
      return lista.head :: getMod(lista.tail, i.+(1), mod)
    else
      return getMod(lista.tail, i.+(1), mod)
  }

  /**************** </TRANSPUESTA> *****************/

  /**
   *  Crea una tabla aleatoria con n columnas e y filas
   */
  def creaTabla(n: Integer, y: Integer): List[List[Char]] = {
    if (n == 0)
      return Nil;
    else
      return creaLista(y) :: creaTabla(n.-(1), y);
  }

  /**
   * Crea una lista aleatoria con n elementos
   */
  def creaLista(n: Integer): List[Char] = {
    if (n == 0)
      return Nil;
    else
      return colores(rnd.nextInt(dificultad(dificultadN))) :: creaLista(n.-(1));
  }

  /**
   * Crea una lista de n elementos y la rellena con caracteres c
   */
  def nuevaLista(c: Char, n: Integer): List[Char] = {
    if (n == 0)
      return Nil
    else
      return c :: nuevaLista(c, n.-(1))
  }
  
  

  /**
   * determina si un intercambio es válido
   */
  
  def isValid(x1:Int,y1:Int,x2:Int,y2:Int): Boolean = {
    if(x1.equals(x2))
      if(Math.abs((y2-y1)).equals(1)) return true
      else return false
    if(y1.equals(y2))
      if(Math.abs((x2-x1)).equals(1)) return true
      else return false
    else return false
    
  }

  /**
   * Comprueba si todos los elementos de una lista son iguales
   * Ponemos como marca que no compruebe si el elemento es 0 porque
   * es el caracter con el que marcaremos los elementos repetidos
   */
  def todosIguales(lista: List[Char]): Boolean = {
    if (lista.isEmpty || lista.tail.isEmpty)
      return true
    else if (lista.head == '0')
      return false
    else
      return lista.head.equals(lista.tail.head) && todosIguales(lista.tail)
  }

  /**
   * Marca con 0 los elementos que hay que eliminar de la lista
   * (con 3 o más coincidencias contiguas)
   */
  def borraCoincidenciasLista(lista: List[Char]): List[Char] = {
    val l1 = borraCoincidenciasAux(lista)
    val l2 = borraCoincidenciasAux(lista.reverse).reverse
    if (cuentaElementosLista('0', l1) > cuentaElementosLista('0', l2)) {
      return l1
    } else {
      return l2
    }
  }

  /**
   * Método auxiliar para ayudar en la recursividad del método borraCoincidenciasLista
   */
  def borraCoincidenciasAux(lista: List[Char]): List[Char] = {
    if (lista.length < 3)
      return lista
    else if (todosIguales(lista))
      return nuevaLista('0', lista.length)
    else
      return lista.head :: borraCoincidenciasLista(lista.tail)
  }

  /**
   * Cuenta el número de repeticiones de c en la lista l
   */
  def cuentaElementosLista(c: Char, lista: List[Char]): Integer = {
    if (lista.isEmpty)
      return 0
    else if (lista.head == c)
      return 1 + (cuentaElementosLista(c, lista.tail))
    else
      return cuentaElementosLista(c, lista.tail)
  }

  /**
   *  Borra los elementos marcados como 0 en una lista
   */
  def compacta(lista: List[Char]): List[Char] = {
    if (lista == Nil)
      return Nil
    else if (lista.head.equals('0'))
      return compacta(lista.tail)
    else
      return lista.head :: compacta(lista.tail)
  }

  /**
   *  Llama al método compacta y rellena la cabeza de la lista con colores aleatorios
   */
  def compactaYRellenaLista(lista: List[Char]): List[Char] = {
    if (lista.isEmpty)
      return Nil
    else {
      val aux = compacta(lista)
      return creaLista(lista.size - aux.size) ::: aux
    }
  }

  /**************** <EXCHANGE> *********************/

  def exchange(tabla: List[List[Char]], x1: Int, y1: Int, x2: Int, y2: Int): List[List[Char]] = {
    if (x1.equals(x2)) {
      if (y1 < y2) return exchange_horizontal(tabla, x1, y1, x2, y2, 0, 0)
      else return exchange_horizontal(tabla, x2, y2, x1, y1, 0, 0)
    } else if (y1.equals(y2)) {
      if (x1 < x2) return traspose(exchange_vertical(traspose(tabla), y1, x1, y2, x2, 0, 0))
      else return traspose(exchange_vertical(traspose(tabla), y2, x2, y1, x1, 0, 0))
    } else return Nil
  }

  def exchange_vertical(tabla: List[List[Char]], x1: Int, y1: Int, x2: Int, y2: Int, xAct: Int, yAct: Int): List[List[Char]] = {
    if (xAct < x1) return tabla.head :: exchange_vertical(tabla.tail, x1, y1, x2, y2, xAct.+(1), yAct)
    else return exchange_row(tabla.head, y1, y2, 0) :: tabla.tail
  }

  def exchange_horizontal(tabla: List[List[Char]], x1: Int, y1: Int, x2: Int, y2: Int, xAct: Int, yAct: Int): List[List[Char]] = {
    if (xAct < x1) return tabla.head :: exchange_horizontal(tabla.tail, x1, y1, x2, y2, xAct.+(1), yAct)
    else return exchange_row(tabla.head, y1, y2, 0) :: tabla.tail
  }

  def exchange_row(row: List[Char], p1: Integer, p2: Integer, current: Integer): List[Char] = {
    if (current < p1) return row.head :: exchange_row(row.tail, p1, p2, current.+(1))
    else return row.tail.head :: row.head :: (row drop 2)
  }

  /**************** </EXCHANGE> *********************/

  /**************** <CHECK> ***********************/

  def check_horizontal(primera: List[Char], resto: List[List[Char]], x: Integer, y: Integer): List[Integer] = {
    if ((!resto.isEmpty) && (!primera.isEmpty))
      if (primera.length <= 2)
        return check_horizontal(resto.head, resto.tail, x + 1, 0)
      else if ((primera.tail takeWhile (_ == primera.head)).length >= 2)
        return List(x, y, (primera.tail takeWhile (_ == primera.head)).length.+(1))
      else {
        val ocurrences = (primera.tail takeWhile (_ == primera.head)).length
        return check_horizontal(primera drop (1 + ocurrences), resto, x, y + 1 + ocurrences)
      }
    else
      return Nil
  }

  def check_vertical(primera: List[Char], resto: List[List[Char]], x: Integer, y: Integer): List[Integer] = {
    if ((!resto.isEmpty) && (!primera.isEmpty))
      if (primera.length <= 2)
        return check_vertical(resto.head, resto.tail, x + 1, 0)
      else if ((primera.tail takeWhile (_ == primera.head)).length >= 2)
        return List(y, x, (primera.tail takeWhile (_ == primera.head)).length.+(1))
      else {
        val ocurrences = (primera.tail takeWhile (_ == primera.head)).length
        return check_vertical(primera drop (1 + ocurrences), resto, x, y + 1 + ocurrences)
      }
    else
      return Nil
  }

  def check(table: List[List[Char]], tipo: Char): List[Integer] = {
    if (tipo.equals('H')) return check_horizontal(table.head, table.tail, 0, 0)
    else if (tipo.equals('V')) {
      val trasposed = traspose(table)
      return check_vertical(trasposed.head, trasposed.tail, 0, 0)
    } else return Nil
  }

  /********************* </CHECK> ****************************************/
  def find_move(tabla: List[List[Char]]): List[Integer] = {
    val moveH = find_move_horizontal(tabla.head, tabla.tail, 'H', 0, 0)
    val trasposed = traspose(tabla)
    val moveV = find_move_vertical(trasposed.head, trasposed.tail, 'V', 0, 0)
    if (moveH != Nil) return moveH
    else if (moveV != Nil) return moveV
    else return Nil

  }
  //-----------------------------------------------------------------//
  def find_move_horizontal(primera: List[Char], resto: List[List[Char]], tipo: Char, x: Integer, y: Integer): List[Integer] = {
    if (primera.length >= 4)
      if ((((primera take 4) filter (_.equals(primera.head))).length).equals(3)) return find_point((primera take 4), x, y, tipo)
      else return find_move_horizontal(primera.tail, resto, tipo, x, y.+(1))
    else if (resto.isEmpty) return Nil
    else return find_move_horizontal(resto.head, resto.tail, tipo, x.+(1), 0)
  }
  //-----------------------------------------------------------------//
  def find_move_vertical(primera: List[Char], resto: List[List[Char]], tipo: Char, x: Integer, y: Integer): List[Integer] = {
    if (primera.length >= 4)
      if ((((primera take 4) filter (_.equals(primera.head))).length).equals(3)) return find_point((primera take 4), x, y, tipo)
      else return find_move_vertical(primera.tail, resto, tipo, x.+(1), y)
    else if (resto.isEmpty) return Nil
    else return find_move_vertical(resto.head, resto.tail, tipo, 0, y.+(1))
  }
  //-----------------------------------------------------------------//
  def find_point(lista: List[Char], x: Integer, y: Integer, tipo: Char): List[Integer] = {
    if (tipo.equals('V'))
      if (!lista.head.equals(lista.tail.head)) return List(x, y, (x + 1), y)
      else return List((x + 2), y, (x + 3), y)
    else if (tipo.equals('H'))
      if (!lista.head.equals(lista.tail.head)) return List(x, y, x, (y + 1))
      else return List(x, y + 2, x, y + 3)
    else return Nil
  }

}
