

object main {
  
  
  
  def imprimir(a:Int,lista:List[Int]) {//a es un contador para saber en que posicion nos encontramos

	if(lista==Nil){
	  
	  println("--------");
	  
	}else if((a+1)%9==0){
	  
	  println(lista.head);
	  imprimir(a+1,lista.tail);
	  
	}else{
	  
  	print(lista.head);
  	print(" ");
  	imprimir(a+1,lista.tail);
  	
	}

}
   //...

   
 def leerColumna(l: List[Int], i: Int, a: Int, long: Int, l2: List[Int]): List[Int] = {
      if(a < long)
              if(l.tail == Nil)
              {
                if((i%long) == a){
                  val lAux = l2 ::: List(l.head)
                    lAux
                }else{
                  val lAux = l2 
                    lAux
                }

              }
              else
                  if((i%long) == a)
                      leerColumna(l.tail, (i+1), a, long, l2 ++ List(l.head))
                  else
                      leerColumna(l.tail, (i+1), a, long, l2)
          else
              List(-1)
  }
     
     
  def leerFila(l: List[Int], i: Int, a: Int, f: Int, l2: List[Int]): List[Int] = {
  	if(a < f)
  		if(l== Nil){
  		  val lAux = l2;
  			lAux;
  		}else
  			if((i/f) == a)
  				leerFila(l.tail, (i+1), a, f, l2 ++ List(l.head))
  			else
  				leerFila(l.tail, (i+1), a, f, l2)
  	else
  		List(-1)
  }
  



def todas(x: Int, y : Int): Int = {
     if(x == y) x
     else 0
     }
  

  
  def seguidos(long : Int,n:Int , fila:List[Int]): Int = {
	val l = fila.length;  //Longitud de la lista
	if(n==long){            //Si es el final de la iteracion para
		0;
	}else{
		val same = fila.reduce(todas)  //Comprueba si toda la lista tiene el mismo numero
		
		if(same != 0){  
		 
			l;
		}else{
		val aux = fila.reverse; //Le da la vuelta a la funcion
		val aux2 = aux.tail; //Se queda con la cola
			seguidos(long,n+1,aux2.reverse); //recursiva dando la vuelta de nuevo
			
			}
	}
}
  
  
  
  
  def matrizFilas(l1: List[Int], l2: List[Int], l3: List[Int], l4: List[Int], l5: List[Int], l6: List[Int], l7: List[Int]): List[Int] = {
	val matriz = List(l1,l2,l3,l4,l5,l6,l7).par.reduce(_++_)
	matriz
}
  
  
  def matrizColumnas(c1: List[Int], c2: List[Int], c3: List[Int], c4: List[Int], c5: List[Int], c6: List[Int], c7: List[Int], c8: List[Int], c9: List[Int], matriz: List[Int]): List[Int] = {
	if(c1.tail == Nil)
	{
		val matrizAux = List(matriz,List(c1.head),List(c2.head),List(c3.head),List(c4.head),List(c5.head),List(c6.head),List(c7.head),List(c8.head),List(c9.head)).par.reduce(_++_)
		matrizAux
	}
	else
	{
    val matrizAux = List(matriz,List(c1.head),List(c2.head),List(c3.head),List(c4.head),List(c5.head),List(c6.head),List(c7.head),List(c8.head),List(c9.head)).par.reduce(_++_)
    matrizColumnas(c1.tail, c2.tail, c3.tail, c4.tail, c5.tail, c6.tail, c7.tail, c8.tail, c9.tail, matrizAux)
  }
}   
  
  def comparaMatrices(l1: List[Int], l2: List[Int], r: List[Int]): List[Int] = {
	if(l1.tail == Nil)
	{
		if(l1.head == l2.head)
		{
			val rfinal = r ++ List(l1.head)
			rfinal
		}
		else
		{
			val rfinal = r ++ List(0)
			rfinal
		}
	}
	else
	{
		if(l1.head == l2.head)
		{
			comparaMatrices(l1.tail, l2.tail, r ++ List(l1.head))
		}
		else
		{
			comparaMatrices(l1.tail, l2.tail, r ++ List(0))
		}
	}
} 
  
 
  
  def generarTablero(n:Int,lista:List[Int]): List[Int] = {
		if(n==0){
			lista
		}else{
			val r = scala.util.Random;
			val color = 1 + r.nextInt(6);
			val aux:List[Int] = List(color);
			lista ::: generarTablero(n-1,lista) ::: aux ;
		}
	}                                         //> generarTablero: (n: Int, lista: List[Int])List[Int]
	
	
	def mover(n :Int ,posicion: Int , movimiento: Int,tablero :List[Int]): List[Int] = {
  	if(posicion>7*9){
  		List(); //Error la posicion no existe
  	}else if (posicion < 0){
  		List(); //Error la posicion no existe
  	}else if(movimiento>7*9){
  		List(); //Error la posicion no existe
  	}else if (movimiento < 0){
  		List(); //Error la posicion no existe
  	}else if(n>=7*9){
  		List();
  	}
  	else if(n==posicion){
  			List(tablero(movimiento)) ::: mover(n+1,posicion,movimiento,tablero);
  		
  	}else if(n==movimiento){
  			List( tablero(posicion)) ::: mover(n+1,posicion,movimiento,tablero);
  	}else{
  			List( tablero(n)) ::: mover(n+1,posicion,movimiento,tablero); //Seguimos buscando la posicion a mover
  			
  				}
  } 
	
	def cambio(posicion :Int , movimiento : Int , tablero: List[Int]): List[Int] ={

  	if(movimiento==2){
  		if(posicion%9 == 0){
  			List();
  		}else{
  			mover(0,posicion,posicion-1,tablero);
  		}
  	}else if(movimiento==3){
  		if(posicion/9 == 6){
  			List();
  		}else{
  			mover(0,posicion,posicion+9,tablero);
  			}
  	}else if(movimiento==4){
  		if((posicion+1)%9 == 0){
  			List();
  		}else{
  		mover(0,posicion,posicion+1,tablero);
  			}
  	}else if(movimiento==1){
  		if(posicion/9 == 0){
  			List();
  		}else{
  			mover(0,posicion,posicion-9,tablero);
  		}
  	}else{
  	List();
  	}

  }
	
	def fila0s(n:Int , fila:List[Int]): List[Int] ={
	if(n==8){
		List();
	}else if( fila(n) == fila(n+1)){
		List(n) ::: fila0s(n+1,fila);
	}else{
		List(n) ::: fila0s(n+1,fila);
		}
  }

 
  
  def filanueva(n:Int , fila:List[Int]) : List[Int] = {
	
    
    
  	if(n==8){
  		List(fila.head);
  	}else{
  		val seg = seguidos(fila.length,0,fila);
 
  		if (seg >2) {
  			val filaux = fila.drop(seg)
  			
  			if(n+seg>8){
  			  
			    List.range(0,seg).map{case _ => 0} 
			}else{
			    List.range(0,seg).map{case _ => 0} ::: filanueva(n+seg,filaux);  
			}
  		}else{
  			List(fila.head) ::: filanueva(n+1,fila.tail);
  		}
  	}
  	
  	
  }
  
  
  
 
  def pedirX(n: Int): Int = {
       if(n == 0){
         println("\n\tFila (1 - 7): ")
         val x = readInt()
         if(x > 7 || x < 1){
           println("Valor no válido: \n")
           pedirX(0)
         }else{
           pedirX(x)          
         }         
       }else{
         n
       }
     }
      def pedirY(n: Int): Int = {
       if(n == 0){
         println("\n\tColumna (1 - 9): ")
         val x = readInt()
         if(x > 9 || x < 1){
           println("Valor no válido: \n")
           pedirY(0)
         }else{
           pedirY(x)          
         }         
       }else{
         n
       }
     }
     def pedirMov(n: Int): Int = {
       if(n == 0){
         println("\n¿Hacia donde desea mover? (W-A-S-D): ")
         val mov = readLine()
         val i = mov match {
         case "w" => 1
         case "a" => 2       
         case "s" => 3
         case "d" => 4
         case "W" => 1
         case "A" => 2       
         case "S" => 3
         case "D" => 4
         case _ => 0
         }
         if(i == 0){
           println("Dirección no válida\n")
           pedirMov(0)           
         }else{
           pedirMov(i)           
         }
       }else{
         n
       }
       
     }
     
     def seguir(n: Int): Int = {
       if(n == 0){
         println("¿Desea seguir jugando? (S/N)")
         val opt = readLine()
         val i = opt match {
           case "s" => 2
           case "S" => 2     
           case "n" => 1
           case "N" => 1
           case _ => -1
           }
         
         if(i == -1){
           seguir(0)
         }else{
           seguir(i)
         }
      }else{
        n        
      }
       
       
     }
  
   def pedirUser() : List[Int]= {
     val opt = seguir(0)
     if(opt == 2){
       println("Seleccione casilla: ")
       val x = pedirX(0) -1
       println("\n\tPosicion y: ")
       val y = pedirY(0) -1  
       val i = pedirMov(0);     
       List(x,y,i,opt)
     }else{
       List(0,0,0,1)
     }
   }
   

  
  def colnueva(n:Int , fila:List[Int]) : List[Int] = {

	if(n==6){
		List(fila.head);
	}else{
		val seg = seguidos(fila.length,0,fila);
		if (seg >2) {
			val filaux = fila.drop(seg)
			if(n+seg>6){
			     List.range(0,seg).map{case _ => 0}
			}else{
			    List.range(0,seg).map{case _ => 0} ::: colnueva(n+seg,filaux);  
			}
			
		}else{
			List(fila.head) ::: colnueva(n+1,fila.tail);
		}
	}
	
	
} 
  
  def actualizaTablero(lst: List[Int]): List[Int] = {
  	val c1 = leerColumna(lst,0,0,9,List())
  	val c2 = leerColumna(lst,0,1,9,List())
  	val c3 = leerColumna(lst,0,2,9,List())
  	val c4 = leerColumna(lst,0,3,9,List())
  	val c5 = leerColumna(lst,0,4,9,List())
  	val c6 = leerColumna(lst,0,5,9,List())
  	val c7 = leerColumna(lst,0,6,9,List())
  	val c8 = leerColumna(lst,0,7,9,List())
  	val c9 = leerColumna(lst,0,8,9,List())
  
  	
  	val aux1 = c1.filter(_ != 0)
  	val f1 =  List.range(0,7 - aux1.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux1
  	
  	val aux2 = c2.filter(_ != 0)
  	val f2 =  List.range(0,7 - aux2.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux2
  
  	val aux3 = c3.filter(_ != 0)
  	val f3 =  List.range(0,7 - aux3.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux3 
  
  	val aux4 = c4.filter(_ != 0)
  	val f4 =  List.range(0,7 - aux4.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux4
  
  	val aux5 = c5.filter(_ != 0)
  	val f5 =  List.range(0,7 - aux5.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux5
  
  	val aux6 = c6.filter(_ != 0)
  	val f6 =  List.range(0,7 - aux6.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux6
  
  	val aux7 = c7.filter(_ != 0)
  	val f7 =  List.range(0,7 - aux7.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux7
  
  	val aux8 = c8.filter(_ != 0)
  	val f8 =  List.range(0,7 - aux8.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux8
  
 
    val aux9 = c9.filter(_ != 0)
  	val f9 =  List.range(0,7 - aux9.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux9

  
  	val lf = matrizColumnas(f1,f2,f3,f4,f5,f6,f7,f8,f9,List())
  	lf
  
  
  
  
  }
  
  def eliminarTres(l: List[Int]): List[Int] = {
		val f1 = leerFila(l, 0, 0, 9, List())
		val f2 = leerFila(l, 0, 1, 9, List())
		val f3 = leerFila(l, 0, 2, 9, List())
		val f4 = leerFila(l, 0, 3, 9, List())
		val f5 = leerFila(l, 0, 4, 9, List())
		val f6 = leerFila(l, 0, 5, 9, List())
		val f7 = leerFila(l, 0, 6, 9, List())


		val c1 = leerColumna(l, 0, 0, 9, List())
		val c2 = leerColumna(l, 0, 1, 9, List())
		val c3 = leerColumna(l, 0, 2, 9, List())
		val c4 = leerColumna(l, 0, 3, 9, List())
		val c5 = leerColumna(l, 0, 4, 9, List())
		val c6 = leerColumna(l, 0, 5, 9, List())
		val c7 = leerColumna(l, 0, 6, 9, List())
		val c8 = leerColumna(l, 0, 7, 9, List())
		val c9 = leerColumna(l, 0, 8, 9, List())
		
	
		
		val a1 = filanueva(0, f1)
		val a2 = filanueva(0, f2)
		val a3 = filanueva(0, f3)
		val a4 = filanueva(0, f4)
		val a5 = filanueva(0, f5)
		val a6 = filanueva(0, f6)
		val a7 = filanueva(0, f7)
		
		val b1 = colnueva(0, c1)
		val b2 = colnueva(0, c2)
		val b3 = colnueva(0, c3)
		val b4 = colnueva(0, c4)
		val b5 = colnueva(0, c5)
		val b6 = colnueva(0, c6)
		val b7 = colnueva(0, c7)
		val b8 = colnueva(0, c8)
		val b9 = colnueva(0, c9)
		
		val a = matrizFilas(a1, a2, a3, a4, a5, a6, a7)
		val b = matrizColumnas(b1, b2, b3, b4, b5, b6, b7, b8, b9, List())

		
	  comparaMatrices(a, b, List())
		
	} 
  
  def turno(control : Int, tablero:List[Int]): List[Int] ={
   
    if(control == 1000) tablero
    else if(control % 25 == 0){
      val a = seguir(0)
      if(a == 2){
        print("Tablero -- Turno: ")
        println(control)
        imprimir(0,tablero)
        val nuevo = bucleTablero(tablero)
        val aux = bestMove(0,(0,0,0),tablero)
        print("Movimiento: ")
        println(aux)
        if(aux._3 == 0){
          println("No hay mas movimientos posibles")
          tablero
        }else{
          val nuevo2 = cambio(aux._1,aux._2,nuevo)
          turno(control + 1,nuevo2)
        }    
 
      }else{tablero}
    }else{
      print("Tablero -- Turno: ")
      println(control)
      imprimir(0,tablero)
      val nuevo = bucleTablero(tablero)
      val aux = bestMove(0,(0,0,0),tablero)
      print("Movimiento: ")
      println(aux)
      if(aux._3 == 0){
        println("No hay mas movimientos posibles")
        tablero
      }else{
        val nuevo2 = cambio(aux._1,aux._2,nuevo)
        turno(control + 1,nuevo2)
      }    
 
    }
     
  }
  
  def bucleTablero(tablero:List[Int]): List[Int] = {
    //eliminar 3 
    
    val nuevo = eliminarTres(tablero);
    imprimir(0,nuevo);
    
    if(tablero == nuevo){
        tablero
    }else{
       val nuevo2 = actualizaTablero(nuevo);
        imprimir(0,nuevo2)
        bucleTablero(nuevo2)
    }
    //rellenar 
  }
  
  
  
  def contar0s(movimiento:Int ,posicion:Int , tablero: List[Int]): Int = {
    
    val tableroaux = cambio(posicion,movimiento,tablero)
    if(tableroaux == List()) 0
    else{
    val aux2 = eliminarTres(tableroaux)
    aux2.count(_==0)
    }
    
  }
  
  
  def posicion0s(posicion:Int, tablero:List[Int]): (Int,Int,Int) = {
    
    val arriba = contar0s(1,posicion,tablero)
    
    
    val abajo = contar0s(3,posicion,tablero)
    
    
    
    val derecha = contar0s(4,posicion,tablero)
    
    
    
    val izquierda = contar0s(2,posicion,tablero)
    
    
    
    val movimientos = List(arriba,abajo,derecha,izquierda)
    
    val maximo = movimientos.max
    /*
    print("Arriba Abajo Derecha Izquierda -- posicion: ")
    println(posicion)
    println(movimientos)
    * */
    

    
    if(arriba == maximo){
      (posicion,1,maximo)
    }else if(abajo == maximo){
      (posicion,3,maximo)
    }else if(derecha == maximo){
       (posicion,4,maximo)
    }else{
      (posicion,2,maximo)
    }
    

  
  }

    
def bestMove(n:Int, t: (Int,Int,Int),tablero:List[Int]): (Int,Int,Int) = {
    if(n == 63) t
    else{
      val aux = posicion0s(n,tablero)
      if(aux._3 > t._3) bestMove(n + 1, aux, tablero)
      else bestMove(n + 1, t, tablero)
    }
} 
   
def main(args:Array[String]) { 
    
    //Declaramos objetos
    
    
    //Generamos el tablero
    
    
    //bucle
    val tablero = generarTablero(7*9, List()); 
    turno(0,tablero)
    
      
  
  
    println("Fin ejecucion")
    
    
    
   
  }
}

