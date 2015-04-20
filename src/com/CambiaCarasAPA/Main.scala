package com.CambiaCarasAPA
import java.util.Scanner;

class main {
  def main(args: Array[String]) {
    print("Escoja dificultad del 1 al 3: ")

    val sc = new Scanner(System.in)
    val dificultad = sc.next().toInt - 1

    val juego = new Juego(dificultad)
    juego.jugar(juego.limpiar(juego.creaTabla(8, 8)))
  }
}