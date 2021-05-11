

object main {
  

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
   
   def leer(n:Int,lista:List[Int]): Int ={//n es la posicion a leer
	if(lista==Nil){
	-1;
	}
	else if(n==0){
	lista.head;
	}else{
	leer(n-1,lista.tail);
	}
	
} 
   
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
  
  def length(lista:List[Int],longitud:Int): Int ={
    
	if(lista == Nil){
	  
		longitud;
		
	}else{
	  
		length(lista.tail,longitud+1);
	}
} 
  def todas(n:Int , fila:List[Int]): Int ={ //Devuelve 1 si todos los numeros de la fila son el mismo, sino devuelve 0
	
  if(fila.tail == Nil){
    0   
  }else if(n+1==length(fila,0)){
		1 }
	else if(leer(n,fila) != leer(n+1,fila)){
		0 }
	else{
		todas(n+1,fila);
	}

}
  
  def reverse(fila:List[Int]) : List[Int] = {

	if(fila==Nil){
		List();
	}else{
		reverse(fila.tail) ::: List(fila.head);
	}

} 
  
  def seguidos(long : Int,n:Int , fila:List[Int]): Int = {
	val l = length(fila,0);  //Longitud de la lista
	if(n==long){            //Si es el final de la iteracion para
		0;
	}else{
		val same = todas(0,fila);  //Comprueba si toda la lista tiene el mismo numero
		
		if(same == 1){  //Si tiene el mismo numero devuelve la longitud de la lista
		 
			l;
		}else{
		val aux = reverse(fila); //Le da la vuelta a la funcion
		val aux2 = aux.tail; //Se queda con la cola
			seguidos(long,n+1,reverse(aux2)); //recursiva dando la vuelta de nuevo
			
			}
	}
}
  
  
  def quitarHead(n:Int , fila:List[Int]) : List[Int] = {
	if (n > 0){
		quitarHead(n-1, fila.tail);
	}else{
	fila;
	
	}
} 
  
  def matrizFilas(l1: List[Int], l2: List[Int], l3: List[Int], l4: List[Int], l5: List[Int], l6: List[Int], l7: List[Int]): List[Int] = {
	val matriz = List() ++ l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7
	matriz
}
  
  
  def matrizColumnas(c1: List[Int], c2: List[Int], c3: List[Int], c4: List[Int], c5: List[Int], c6: List[Int], c7: List[Int], c8: List[Int], c9: List[Int], matriz: List[Int]): List[Int] = {
	if(c1.tail == Nil)
	{
		val matrizAux = matriz ++ List(c1.head) ++ List(c2.head) ++ List(c3.head) ++ List(c4.head) ++ List(c5.head) ++ List(c6.head) ++ List(c7.head) ++ List(c8.head) ++ List(c9.head)
		matrizAux
	}
	else
	{
		val matrizAux = matriz ++ List(c1.head) ++ List(c2.head) ++ List(c3.head) ++ List(c4.head) ++ List(c5.head) ++ List(c6.head) ++ List(c7.head) ++ List(c8.head) ++ List(c9.head)
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
	val tablero = generarTablero(7*9,List());
	
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
  			List(leer(movimiento,tablero)) ::: mover(n+1,posicion,movimiento,tablero);
  		
  	}else if(n==movimiento){
  			List( leer(posicion,tablero)) ::: mover(n+1,posicion,movimiento,tablero);
  	}else{
  			List( leer(n,tablero)) ::: mover(n+1,posicion,movimiento,tablero); //Seguimos buscando la posicion a mover
  			
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
	}else if( leer(n,fila) == leer(n+1,fila)){
		List(n) ::: fila0s(n+1,fila);
	}else{
		List(n) ::: fila0s(n+1,fila);
		}
  }

  def generar0s(n:Int,lista:List[Int] ): List[Int] = { //Lista de enteros de 1 a n

		if(n==0){
			lista;
			}else{
			lista ::: generar0s(n-1,lista) ::: List(0);
			}
		

  }
  
  def filanueva(n:Int , fila:List[Int]) : List[Int] = {
	
    
    
  	if(n==8){
  		List(fila.head);
  	}else{
  		val seg = seguidos(length(fila,0),0,fila);
 
  		if (seg >2) {
  			val filaux = quitarHead(seg,fila);
  			
  			if(n+seg>8){
  			  
			    generar0s(seg,List());  
			}else{
			    generar0s(seg,List()) ::: filanueva(n+seg,filaux);  
			}
  		}else{
  			List(fila.head) ::: filanueva(n+1,fila.tail);
  		}
  	}
  	
  	
  }
  
  def removeZeros(lst: List[Int],acc: List[Int]): List[Int] = {

    if(lst == List())
    {
        acc
    }else{
        if(lst.head == 0){
            removeZeros(lst.tail,acc)
        }else{
            removeZeros(lst.tail,acc ::: List(lst.head))
        }
    }
}

  
  def anadirRec(lst: List[Int], n: Int): List[Int] = {
          if(n <= 0) {
              lst
          }else{
          val r = scala.util.Random;
          val color = 1 + r.nextInt(6);
          anadirRec(List(color) ::: lst, n - 1)
  			}
      }
  
  
  def anadir(lst: List[Int]): List[Int] = {
      val lt = length(lst,0)
      anadirRec(lst,7 - lt)
  		
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
		val seg = seguidos(length(fila,0),0,fila);
		if (seg >2) {
			val filaux = quitarHead(seg,fila);
			if(n+seg>6){
			     generar0s(seg,List());  
			}else{
			    generar0s(seg,List()) ::: colnueva(n+seg,filaux);  
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
  
  	val aux1 = removeZeros(c1, List())
  	val f1 = anadir(aux1)
  	
  	val aux2 = removeZeros(c2, List())
  	val f2 = anadir(aux2)
  
  	val aux3 = removeZeros(c3, List())
  	val f3 = anadir(aux3)
  
  	val aux4 = removeZeros(c4, List())
  	val f4 = anadir(aux4)
  
  	val aux5 = removeZeros(c5, List())
  	val f5 = anadir(aux5)
  
  	val aux6 = removeZeros(c6, List())
  	val f6 = anadir(aux6)
  
  	val aux7 = removeZeros(c7, List())
  	val f7 = anadir(aux7)
  
  	val aux8 = removeZeros(c8, List())
  	val f8 = anadir(aux8)
  
 
  	val aux9 = removeZeros(c9, List())

  	val f9 = anadir(aux9)

  
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
    
    println("Tablero")
    imprimir(0,tablero)
    
    if(control==2){
      val nuevo = bucleTablero(tablero)
      
      val parametros = pedirUser()
      val posicion =leer(0,parametros)*9 + leer(1,parametros)
      val direccion = leer(2,parametros)
      if(leer(3,parametros)==2){
 
        val nuevo2 = cambio(posicion,direccion,nuevo)
        imprimir(0,nuevo2)
        if(nuevo2 == Nil){
          println("movimiento no valido")
          turno(2,nuevo)
        }else{
          turno(2,nuevo2)
        }
        
      }else{
        turno(1,nuevo)
      }
      
    }else if (control ==1 ){
      tablero
    }else{
      tablero
    }
  }
  
  def bucleTablero(tablero:List[Int]): List[Int] ={
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
    
   
def main(args:Array[String]) { 
    
    //Declaramos objetos
    
    
    //Generamos el tablero
    val tablero = generarTablero(7*9, List());   
   
    //bucle
    
    turno(2,tablero)
    
    println("Fin ejecucion")
    
  }
}

}