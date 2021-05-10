

class Auxiliares {
  
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
   
     def leerColumna(l: List[Int], i: Int, a: Int, long: Int, l2: List[Int]): List[Int] = {
	  if(a < long)
	  		if(l == Nil)
	  		{
	  			val lAux = l2 
	  			lAux
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
	else if(fila(n) != fila(n+1)){
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
  
  
}