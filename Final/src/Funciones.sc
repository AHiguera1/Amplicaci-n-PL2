object Funciones {
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

	var tablero = generarTablero(7*9,List()); //> tablero  : List[Int] = List(2, 6, 6, 6, 4, 3, 3, 6, 3, 4, 6, 3, 3, 4, 3, 5, 
                                                  //| 4, 6, 1, 3, 3, 2, 5, 2, 5, 2, 4, 4, 6, 1, 3, 3, 2, 3, 1, 2, 6, 5, 5, 5, 5, 1
                                                  //| , 1, 4, 2, 3, 6, 2, 1, 1, 1, 6, 4, 3, 5, 1, 6, 4, 6, 2, 3, 1, 5)
	imprimir(0,tablero)                       //> 2 6 6 6 4 3 3 6 3
                                                  //| 4 6 3 3 4 3 5 4 6
                                                  //| 1 3 3 2 5 2 5 2 4
                                                  //| 4 6 1 3 3 2 3 1 2
                                                  //| 6 5 5 5 5 1 1 4 2
                                                  //| 3 6 2 1 1 1 6 4 3
                                                  //| 5 1 6 4 6 2 3 1 5
                                                  //| --------


  
  
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

}                                                 //> imprimir: (a: Int, lista: List[Int])Unit
  

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
}                                                 //> mover: (n: Int, posicion: Int, movimiento: Int, tablero: List[Int])List[Int
                                                  //| ]


mover(0,62,8,tablero);                            //> res0: List[Int] = List(2, 6, 6, 6, 4, 3, 3, 6, 5, 4, 6, 3, 3, 4, 3, 5, 4, 6
                                                  //| , 1, 3, 3, 2, 5, 2, 5, 2, 4, 4, 6, 1, 3, 3, 2, 3, 1, 2, 6, 5, 5, 5, 5, 1, 1
                                                  //| , 4, 2, 3, 6, 2, 1, 1, 1, 6, 4, 3, 5, 1, 6, 4, 6, 2, 3, 1, 3)


    
  def leerColumna(l: List[Int], i: Int, a: Int, long: Int, l2: List[Int]): List[Int] = {
	  if(a < long)
	  		if(l.tail == Nil)
	  		{
	  			val lAux = l2 ++ List(l.head)
	  			lAux
	  		}
	  		else
	  			if((i%long) == a)
	  				leerColumna(l.tail, (i+1), a, long, l2 ++ List(l.head))
	  			else
	  				leerColumna(l.tail, (i+1), a, long, l2)
	  	else
	  		List(-1)
  }                                               //> leerColumna: (l: List[Int], i: Int, a: Int, long: Int, l2: List[Int])List[I
                                                  //| nt]

mover(0,62,8,tablero);                            //> res1: List[Int] = List(2, 6, 6, 6, 4, 3, 3, 6, 5, 4, 6, 3, 3, 4, 3, 5, 4, 6
                                                  //| , 1, 3, 3, 2, 5, 2, 5, 2, 4, 4, 6, 1, 3, 3, 2, 3, 1, 2, 6, 5, 5, 5, 5, 1, 1
                                                  //| , 4, 2, 3, 6, 2, 1, 1, 1, 6, 4, 3, 5, 1, 6, 4, 6, 2, 3, 1, 3)

    
	
	
  
  
  
  
def cambio(posicion :Int , movimiento : String , tablero: List[Int]): List[Int] ={

	if(movimiento=="a"){
		if(posicion%9 == 0){
			List();
		}else{
			mover(0,posicion,posicion-1,tablero);
		}
	}else if(movimiento=="s"){
		if(posicion/9 == 6){
			List();
		}else{
			mover(0,posicion,posicion+9,tablero);
			}
	}else if(movimiento=="d"){
		if((posicion+1)%9 == 0){
			List();
		}else{
		mover(0,posicion,posicion+1,tablero);
			}
	}else if(movimiento=="w"){
		if(posicion/9 == 0){
			List();
		}else{
			mover(0,posicion,posicion-9,tablero);
		}
	}else{
	List();
	}

}                                                 //> cambio: (posicion: Int, movimiento: String, tablero: List[Int])List[Int]

var cambiado = cambio(12,"d",tablero);            //> cambiado  : List[Int] = List(2, 6, 6, 6, 4, 3, 3, 6, 3, 4, 6, 3, 4, 3, 3, 5
                                                  //| , 4, 6, 1, 3, 3, 2, 5, 2, 5, 2, 4, 4, 6, 1, 3, 3, 2, 3, 1, 2, 6, 5, 5, 5, 5
                                                  //| , 1, 1, 4, 2, 3, 6, 2, 1, 1, 1, 6, 4, 3, 5, 1, 6, 4, 6, 2, 3, 1, 5)

imprimir(0,cambiado);                             //> 2 6 6 6 4 3 3 6 3
                                                  //| 4 6 3 4 3 3 5 4 6
                                                  //| 1 3 3 2 5 2 5 2 4
                                                  //| 4 6 1 3 3 2 3 1 2
                                                  //| 6 5 5 5 5 1 1 4 2
                                                  //| 3 6 2 1 1 1 6 4 3
                                                  //| 5 1 6 4 6 2 3 1 5
                                                  //| --------


    
  def leerFila(l: List[Int], i: Int, a: Int, f: Int, l2: List[Int]): List[Int] = {
  	if(a < f)
  		if(l.tail == Nil)
  			l2
  		else
  			if((i/f) == a)
  				leerFila(l.tail, (i+1), a, f, l2 ++ List(l.head))
  			else
  				leerFila(l.tail, (i+1), a, f, l2)
  	else
  		List(-1)
  }                                               //> leerFila: (l: List[Int], i: Int, a: Int, f: Int, l2: List[Int])List[Int]
  

 
  leerFila(cambiado, 0, 5, 9, List())             //> res2: List[Int] = List(3, 6, 2, 1, 1, 1, 6, 4, 3)
  leerColumna(cambiado, 0, 1, 9, List())          //> res3: List[Int] = List(6, 6, 3, 6, 5, 6, 1, 5)
  leerColumna(cambiado, 0, 8, 9, List())          //> res4: List[Int] = List(3, 6, 4, 2, 2, 3, 5)

  

def fila0s(n:Int , fila:List[Int]): List[Int] ={
	if(n==8){
		List();
	}else if( fila(n) == fila(n+1)){
		List(n) ::: fila0s(n+1,fila);
	}else{
		List(n) ::: fila0s(n+1,fila);
		}
}                                                 //> fila0s: (n: Int, fila: List[Int])List[Int]

def generar0s(n:Int,lista:List[Int] ): List[Int] = { //Lista de enteros de 1 a n

		if(n==0){
			lista;
			}else{
			lista ::: generar0s(n-1,lista) ::: List(0);
			}
		

}                                                 //> generar0s: (n: Int, lista: List[Int])List[Int]

generar0s(2,List());                              //> res5: List[Int] = List(0, 0)



def length(lista:List[Int],longitud:Int): Int ={
	if(lista == Nil){
		longitud;}
	else{
		length(lista.tail,longitud+1);
	}
}                                                 //> length: (lista: List[Int], longitud: Int)Int
var l = length(List(1,1,1),0);                    //> l  : Int = 3

def todas(n:Int , fila:List[Int]): Int ={ //Devuelve 1 si todos los numeros de la fila son el mismo, sino devuelve 0
	
	if(n+1==length(fila,0)){
		1 }
	else if(fila(n) != fila(n+1)){
		0 }
	else{
		todas(n+1,fila);
	}

}                                                 //> todas: (n: Int, fila: List[Int])Int
todas(0,List(1,1,1,2,3));                         //> res6: Int = 0

def reverse(fila:List[Int]) : List[Int] = {

	if(fila==Nil){
		List();
	}else{
		reverse(fila.tail) ::: List(fila.head);
	}

}                                                 //> reverse: (fila: List[Int])List[Int]

def seguidos(n:Int , fila:List[Int]): Int = {
	var l = length(fila,0);
	if(n==8){
		0;
	}else{
		var same = todas(0,fila);
		if(same == 1){
			l;
		}else{
		var aux = reverse(fila);
		var aux2 = aux.tail;
			seguidos(n+1,reverse(aux2));
			}
	}
}                                                 //> seguidos: (n: Int, fila: List[Int])Int

seguidos(0,List(4,4,4,7));                        //> res7: Int = 3


def quitarHead(n:Int , fila:List[Int]) : List[Int] = {
	if (n > 0){
		quitarHead(n-1, fila.tail);
	}else{
	fila;
	
	}
}                                                 //> quitarHead: (n: Int, fila: List[Int])List[Int]

quitarHead(2,List(1,1,2));                        //> res8: List[Int] = List(2)

def filanueva(n:Int , fila:List[Int]) : List[Int] = {
	

	if(n==8){
		List(fila.head);
	}else{
		var seg = seguidos(0,fila);
		if (seg >2) {
			var filaux = quitarHead(seg,fila);
			generar0s(seg,List()) ::: filanueva(n+seg,filaux);
		}else{
			List(fila.head) ::: filanueva(n+1,fila.tail);
		}
	}
	
	
}                                                 //> filanueva: (n: Int, fila: List[Int])List[Int]

filanueva(0,List(1,1,1,2,3,4,4,4,7));             //> res9: List[Int] = List(0, 0, 0, 2, 3, 0, 0, 0, 7)



//Funcion que transforma 7 fila en una matriz
def matrizFilas(l1: List[Int], l2: List[Int], l3: List[Int], l4: List[Int], l5: List[Int], l6: List[Int], l7: List[Int]): List[Int] = {
	val matriz = List() ++ l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7
	matriz
}                                                 //> matrizFilas: (l1: List[Int], l2: List[Int], l3: List[Int], l4: List[Int], l
                                                  //| 5: List[Int], l6: List[Int], l7: List[Int])List[Int]

//Funcion que transforma 9 columnas en una matriz
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
}                                                 //> matrizColumnas: (c1: List[Int], c2: List[Int], c3: List[Int], c4: List[Int]
                                                  //| , c5: List[Int], c6: List[Int], c7: List[Int], c8: List[Int], c9: List[Int]
                                                  //| , matriz: List[Int])List[Int]
//Funcion que compara dos matrices
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
}                                                 //> comparaMatrices: (l1: List[Int], l2: List[Int], r: List[Int])List[Int]

comparaMatrices(List(8,0,7,9), List(8,6,0,9), List())
                                                  //> res10: List[Int] = List(8, 0, 0, 9)


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
}                                                 //> removeZeros: (lst: List[Int], acc: List[Int])List[Int]
val sinceros = removeZeros(List(0,1,3,5,3,0,1), List())
                                                  //> sinceros  : List[Int] = List(1, 3, 5, 3, 1)
removeZeros(List(7,7,7,7,7,7,7), List())          //> res11: List[Int] = List(7, 7, 7, 7, 7, 7, 7)

def anadirRec(lst: List[Int], n: Int): List[Int] = {
        if(n <= 0) {
            lst
        }else{
        val r = scala.util.Random;
        val color = 1 + r.nextInt(6);
        anadirRec(List(color) ::: lst, n - 1)
			}
    }                                             //> anadirRec: (lst: List[Int], n: Int)List[Int]


def anadir(lst: List[Int]): List[Int] = {
    val lt = length(lst,0)
    anadirRec(lst,7 - lt)
		
}                                                 //> anadir: (lst: List[Int])List[Int]

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

	println(c9)
	val aux9 = removeZeros(c9, List())
	println(aux9)
	val f9 = anadir(aux9)
	println(f9)

	val lf = matrizColumnas(f1,f2,f3,f4,f5,f6,f7,f8,f9,List())
	lf




}                                                 //> actualizaTablero: (lst: List[Int])List[Int]
imprimir(0, actualizaTablero(matrizFilas(filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)))))
                                                  //> List(7, 7, 7, 7, 7, 7, 7)
                                                  //| List(7, 7, 7, 7, 7, 7, 7)
                                                  //| List(7, 7, 7, 7, 7, 7, 7)
                                                  //| 2 3 2 2 3 1 4 1 7
                                                  //| 2 5 6 2 3 6 2 5 7
                                                  //| 2 2 5 2 3 3 1 1 7
                                                  //| 6 6 3 2 3 1 6 3 7
                                                  //| 6 4 3 2 3 4 3 1 7
                                                  //| 1 4 2 2 3 4 6 3 7
                                                  //| 7 7 7 2 3 7 7 7 7
                                                  //| --------


anadir(sinceros);                                 //> res12: List[Int] = List(3, 5, 1, 3, 5, 3, 1)





}