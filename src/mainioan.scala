package com.CambiaCaras
import scala.util.Random

object Main {
  
  val colores = 'A' :: 'N'  :: 'R' :: 'V' :: 'M' :: 'G' :: 'B' :: Nil
  val dificultad = 3 :: 5 :: 7 :: Nil
  val rnd = new Random()
  val dificultadN = 0
  val dddd = colores take 2;
  val sizeX = 8
  val sizeY = 8
  val test = ('A' :: 'N' :: 'N' :: Nil).intersect(('N'::'P'::Nil))
  
//-----------------------------------------------------------------//
  def main(args: Array[String]) {
    val lista = 2 :: 2 :: 3 :: 4 :: Nil
    val tabla = List.fill(sizeX,sizeY)(colores(rnd.nextInt(dificultad(dificultadN))))
    printa(tabla)
    print(check_horizontal(tabla.head,tabla.tail,0,0))
   // print(tabla.flatten)
   // print("\n")
    val trasposed = traspose(tabla)
    print(check_horizontal(trasposed.head,trasposed.tail,0,0))
  }
//-----------------------------------------------------------------//

//-----------------------------------------------------------------//
  def creaLista(n: Integer): List[Char] = {
    if (n == 0)
      return Nil;
    else
      return colores(rnd.nextInt(dificultad(dificultadN))) :: creaLista(n.-(1));
  }
//-----------------------------------------------------------------//
//-----------------------------------------------------------------//
  def recolocar(p: List[Char], comienzo: Integer,longitud: Integer,fila: Boolean): List[Char] = {
    return p take (comienzo-1)
  }
//-----------------------------------------------------------------//  
  def printa(tabla: List[List[Char]]) = {
    for (x <- tabla) {
      printf("[ ")
      for (y <- x) {
        printf(y.toString() + " ")
      }
      println("]")
    }
  }
  //-----------------------------------------------------------------// 
  def tableToList(tabla: List[List[Char]], i: Integer): List[List[Char]] = {
    if(tabla.isEmpty) return Nil
    else Nil
  }
  //-----------------------------------------------------------------// 
  def getMod(lista: List[Char], i: Integer, mod: Integer):List[Char] = {
    if(lista.isEmpty) return Nil
    else {
      if(i.%(8) == mod) return lista.head :: getMod(lista.tail,i.+(1),mod)
      else return getMod(lista.tail,i.+(1),mod)
    }
  }
  //-----------------------------------------------------------------// 
 def traspose(tabla: List[List[Char]]):List[List[Char]] = {
   val list = tabla.flatten
   return List(getMod(list,0,0),
               getMod(list,0,1),
               getMod(list,0,2),
               getMod(list,0,3),
               getMod(list,0,4),
               getMod(list,0,5),
               getMod(list,0,6),
               getMod(list,0,7))
   
 } 
  //-----------------------------------------------------------------// 
 def check_horizontal(primera: List[Char],resto: List[List[Char]],x:Integer,y:Integer): List[Integer] = {
   if((!resto.isEmpty)&&(!primera.isEmpty)) 
     if(primera.length <= 2)
       return check_horizontal(resto.head,resto.tail,x+1,0)
     else
       if((primera.tail takeWhile (_ == primera.head)).length >= 2)
         return List(x,y)
       else{
         val ocurrences = (primera.tail takeWhile (_ == primera.head)).length
         return check_horizontal(primera drop (1+ocurrences), resto,x,y+1+ocurrences)
       }
   else 
     return Nil
 }
   //-----------------------------------------------------------------//
}
