import scala.collection.parallel.immutable._

object main {
  
  
  
  def imprimir(a:Int,lista:ParSeq[Int]) {//a es un contador para saber en que posicion nos encontramos

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

   
     def leerColumna(l: ParSeq[Int], i: Int, a: Int, long: Int, l2: ParSeq[Int]): ParSeq[Int] = {
	  if(a < long)
	  		if(l.tail == Nil)
	  		{
	  		  if((i%long) == a){
	  		    val lAux = l2 ++ ParSeq(l.head)
	  			  lAux
	  		  }else{
	  		    val lAux = l2 
	  			  lAux
	  		  }
	  			
	  		}
	  		else
	  			if((i%long) == a)
	  				leerColumna(l.tail, (i+1), a, long, l2 ++ ParSeq(l.head))
	  			else
	  				leerColumna(l.tail, (i+1), a, long, l2)
	  	else
	  		ParSeq(-1)
  }
     
     
  def leerFila(l: ParSeq[Int], i: Int, a: Int, f: Int, l2: ParSeq[Int]): ParSeq[Int] = {
  	if(a < f)
  		if(l== Nil){
  		  val lAux = l2;
  			lAux;
  		}else
  			if((i/f) == a)
  				leerFila(l.tail, (i+1), a, f, l2 ++ ParSeq(l.head))
  			else
  				leerFila(l.tail, (i+1), a, f, l2)
  	else
  		ParSeq(-1)
  }
  



def todas(x: Int, y : Int): Int = {
     if(x == y) x
     else 0
     }
  

 def seguidos(long : Int,n:Int , fila:ParSeq[Int]): Int = {
	val l = fila.length;  //Longitud de la lista
	if(n==long){            //Si es el final de la iteracion para
		0;
	}else{
		val same = fila.reduce(todas)  //Comprueba si toda la lista tiene el mismo numero
		
		if(same != 0){  //Si tiene el mismo numero devuelve la longitud de la lista
		 
			l;
		}else{
		val aux = fila.reverse; //Le da la vuelta a la funcion
		val aux2 = aux.tail; //Se queda con la cola
			seguidos(long,n+1,aux2.reverse); //recursiva dando la vuelta de nuevo
			
			}
	}
}
  
  
  
  
  def matrizFilas(l1: ParSeq[Int], l2: ParSeq[Int], l3: ParSeq[Int], l4: ParSeq[Int], l5: ParSeq[Int], l6: ParSeq[Int], l7: ParSeq[Int]): ParSeq[Int] = {
	val matriz = ParSeq(l1,l2,l3,l4,l5,l6,l7).reduce(_++_)
	matriz
}
  
  
  def matrizColumnas(c1: ParSeq[Int], c2: ParSeq[Int], c3: ParSeq[Int], c4: ParSeq[Int], c5: ParSeq[Int], c6: ParSeq[Int], c7: ParSeq[Int], c8: ParSeq[Int], c9: ParSeq[Int], matriz: ParSeq[Int]): ParSeq[Int] = {
	if(c1.tail == Nil)
	{
		val matrizAux = ParSeq(matriz,ParSeq(c1.head),ParSeq(c2.head),ParSeq(c3.head),ParSeq(c4.head),ParSeq(c5.head),ParSeq(c6.head),ParSeq(c7.head),ParSeq(c8.head),ParSeq(c9.head)).reduce(_++_)
		matrizAux
	}
	else
	{
    val matrizAux = ParSeq(matriz,ParSeq(c1.head),ParSeq(c2.head),ParSeq(c3.head),ParSeq(c4.head),ParSeq(c5.head),ParSeq(c6.head),ParSeq(c7.head),ParSeq(c8.head),ParSeq(c9.head))reduce(_++_)
    matrizColumnas(c1.tail, c2.tail, c3.tail, c4.tail, c5.tail, c6.tail, c7.tail, c8.tail, c9.tail, matrizAux)
  }
}   
  
  def comparaMatrices(l1: ParSeq[Int], l2: ParSeq[Int], r: ParSeq[Int]): ParSeq[Int] = {
	if(l1.tail == Nil)
	{
		if(l1.head == l2.head)
		{
			val rfinal = r ++ ParSeq(l1.head)
			rfinal
		}
		else
		{
			val rfinal = r ++ ParSeq(0)
			rfinal
		}
	}
	else
	{
		if(l1.head == l2.head)
		{
			comparaMatrices(l1.tail, l2.tail, r ++ ParSeq(l1.head))
		}
		else
		{
			comparaMatrices(l1.tail, l2.tail, r ++ ParSeq(0))
		}
	}
} 
  
 
  
  def generarTablero(n:Int,lista:ParSeq[Int]): ParSeq[Int] = {
		if(n==0){
			lista
		}else{
			val r = scala.util.Random;
			val color = 1 + r.nextInt(6);
			val aux:ParSeq[Int] = ParSeq(color);
			lista ++ generarTablero(n-1,lista) ++ aux ;
		}
	}                                         //> generarTablero: (n: Int, lista: List[Int])List[Int]
	
	
	def mover(n :Int ,posicion: Int , movimiento: Int,tablero :ParSeq[Int]): ParSeq[Int] = {
  	if(posicion>7*9){
  		ParSeq(); //Error la posicion no existe
  	}else if (posicion < 0){
  		ParSeq(); //Error la posicion no existe
  	}else if(movimiento>7*9){
  		ParSeq(); //Error la posicion no existe
  	}else if (movimiento < 0){
  		ParSeq(); //Error la posicion no existe
  	}else if(n>=7*9){
  		ParSeq();
  	}
  	else if(n==posicion){
  			ParSeq(tablero(movimiento)) ++ mover(n+1,posicion,movimiento,tablero);
  		
  	}else if(n==movimiento){
  			ParSeq(tablero(posicion)) ++ mover(n+1,posicion,movimiento,tablero);
  	}else{
  			ParSeq(tablero(n)) ++ mover(n+1,posicion,movimiento,tablero); //Seguimos buscando la posicion a mover
  			
  				}
  } 
	
	def cambio(posicion :Int , movimiento : Int , tablero: ParSeq[Int]): ParSeq[Int] ={

  	if(movimiento==2){
  		if(posicion%9 == 0){
  			ParSeq();
  		}else{
  			mover(0,posicion,posicion-1,tablero);
  		}
  	}else if(movimiento==3){
  		if(posicion/9 == 6){
  			ParSeq();
  		}else{
  			mover(0,posicion,posicion+9,tablero);
  			}
  	}else if(movimiento==4){
  		if((posicion+1)%9 == 0){
  			ParSeq();
  		}else{
  		mover(0,posicion,posicion+1,tablero);
  			}
  	}else if(movimiento==1){
  		if(posicion/9 == 0){
  			ParSeq();
  		}else{
  			mover(0,posicion,posicion-9,tablero);
  		}
  	}else{
  	ParSeq();
  	}

  }
	
	def fila0s(n:Int , fila:ParSeq[Int]): ParSeq[Int] ={
	if(n==8){
		ParSeq();
	}else if( fila(n) == fila(n+1)){
		ParSeq(n) ++ fila0s(n+1,fila);
	}else{
		ParSeq(n) ++ fila0s(n+1,fila);
		}
  }

 
  
  def filanueva(n:Int , fila:ParSeq[Int]) : ParSeq[Int] = {
	
    
    
  	if(n==8){
  		ParSeq(fila.head);
  	}else{
  		val seg = seguidos(fila.length,0,fila);
 
  		if (seg >2) {
  			val filaux = fila.drop(seg)
  			
  			if(n+seg>8){
  			  
			    ParSeq.range(0,seg).map{case _ => 0} 
			}else{
			    ParSeq.range(0,seg).map{case _ => 0} ++ filanueva(n+seg,filaux);  
			}
  		}else{
  			ParSeq(fila.head) ++ filanueva(n+1,fila.tail);
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
  
   def pedirUser() : ParSeq[Int]= {
     val opt = seguir(0)
     if(opt == 2){
       println("Seleccione casilla: ")
       val x = pedirX(0) -1
       println("\n\tPosicion y: ")
       val y = pedirY(0) -1  
       val i = pedirMov(0);     
       ParSeq(x,y,i,opt)
     }else{
       ParSeq(0,0,0,1)
     }
   }
   

  
  def colnueva(n:Int , fila:ParSeq[Int]) : ParSeq[Int] = {

	if(n==6){
		ParSeq(fila.head);
	}else{
		val seg = seguidos(fila.length,0,fila);
		if (seg >2) {
			val filaux = fila.drop(seg)
			if(n+seg>6){
			     ParSeq.range(0,seg).map{case _ => 0}
			}else{
			    ParSeq.range(0,seg).map{case _ => 0} ++ colnueva(n+seg,filaux);  
			}
			
		}else{
			ParSeq(fila.head) ++ colnueva(n+1,fila.tail);
		}
	}
	
	
} 
  
  def actualizaTablero(lst: ParSeq[Int]): ParSeq[Int] = {
  	val c1 = leerColumna(lst,0,0,9,ParSeq())
  	val c2 = leerColumna(lst,0,1,9,ParSeq())
  	val c3 = leerColumna(lst,0,2,9,ParSeq())
  	val c4 = leerColumna(lst,0,3,9,ParSeq())
  	val c5 = leerColumna(lst,0,4,9,ParSeq())
  	val c6 = leerColumna(lst,0,5,9,ParSeq())
  	val c7 = leerColumna(lst,0,6,9,ParSeq())
  	val c8 = leerColumna(lst,0,7,9,ParSeq())
  	val c9 = leerColumna(lst,0,8,9,ParSeq())
  
  	
  	val aux1 = c1.filter(_ != 0)
  	val f1 =  ParSeq.range(0,7 - aux1.length).map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux1
  	
  	val aux2 = c2.par.filter(_ != 0)
  	val f2 =  ParSeq.range(0,7 - aux2.length).par.map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux2
  
  	val aux3 = c3.par.filter(_ != 0)
  	val f3 =  ParSeq.range(0,7 - aux3.length).par.map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux3
  
  	val aux4 = c4.par.filter(_ != 0)
  	val f4 =  ParSeq.range(0,7 - aux4.length).par.map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux4
  
  	val aux5 = c5.par.filter(_ != 0)
  	val f5 =  ParSeq.range(0,7 - aux5.length).par.map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux5
  
  	val aux6 = c6.par.filter(_ != 0)
  	val f6 =  ParSeq.range(0,7 - aux6.length).par.map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux6
  
  	val aux7 = c7.par.filter(_ != 0)
  	val f7 = ParSeq.range(0,7 - aux7.length).par.map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux7
  
  	val aux8 = c8.par.filter(_ != 0)
  	val f8 = ParSeq.range(0,7 - aux8.length).par.map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux8
 
    val aux9 = c9.par.filter(_ != 0)
  	val f9 =  ParSeq.range(0,7 - aux9.length).par.map{case _ => 1 + scala.util.Random.nextInt(6)} ++ aux9

  
  	val lf = matrizColumnas(f1,f2,f3,f4,f5,f6,f7,f8,f9,ParSeq())
  	lf
  
  
  
  
  }
  
  def eliminarTres(l: ParSeq[Int]): ParSeq[Int] = {
		val f1 = leerFila(l, 0, 0, 9, ParSeq())
		val f2 = leerFila(l, 0, 1, 9, ParSeq())
		val f3 = leerFila(l, 0, 2, 9, ParSeq())
		val f4 = leerFila(l, 0, 3, 9, ParSeq())
		val f5 = leerFila(l, 0, 4, 9, ParSeq())
		val f6 = leerFila(l, 0, 5, 9, ParSeq())
		val f7 = leerFila(l, 0, 6, 9, ParSeq())


		val c1 = leerColumna(l, 0, 0, 9, ParSeq())
		val c2 = leerColumna(l, 0, 1, 9, ParSeq())
		val c3 = leerColumna(l, 0, 2, 9, ParSeq())
		val c4 = leerColumna(l, 0, 3, 9, ParSeq())
		val c5 = leerColumna(l, 0, 4, 9, ParSeq())
		val c6 = leerColumna(l, 0, 5, 9, ParSeq())
		val c7 = leerColumna(l, 0, 6, 9, ParSeq())
		val c8 = leerColumna(l, 0, 7, 9, ParSeq())
		val c9 = leerColumna(l, 0, 8, 9, ParSeq())
			
		
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
		val b = matrizColumnas(b1, b2, b3, b4, b5, b6, b7, b8, b9, ParSeq())

		
	  comparaMatrices(a, b, ParSeq())
		
	} 
  
  def turno(control : Int, tablero:ParSeq[Int]): ParSeq[Int] ={
    
    println("Tablero")
    imprimir(0,tablero)
    
    if(control==2){
      val nuevo = bucleTablero(tablero)
      
      val parametros = pedirUser()
      val posicion =parametros(0)*9 + parametros(1)
      val direccion = parametros(2)
      if(parametros(3)==2){
 
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
  
  def bucleTablero(tablero:ParSeq[Int]): ParSeq[Int] ={
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
    val tablero = generarTablero(7*9, ParSeq());   
   
    //bucle
    
    turno(2,tablero)
    
    println("Fin ejecucion")
    
    


   
  }
}

