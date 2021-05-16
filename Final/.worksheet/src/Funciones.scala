object Funciones {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(261); 
	def generarTablero(n:Int,lista:List[Int]): List[Int] = {
		if(n==0){
			lista
		}else{
			val r = scala.util.Random;
			val color = 1 + r.nextInt(6);
			val aux:List[Int] = List(color);
			lista ::: generarTablero(n-1,lista) ::: aux ;
		}
	};System.out.println("""generarTablero: (n: Int, lista: List[Int])List[Int]""");$skip(44); 

	var tablero = generarTablero(7*9,List());System.out.println("""tablero  : List[Int] = """ + $show(tablero ));$skip(21); ;
	imprimir(0,tablero);$skip(298); 


  
  
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

};System.out.println("""imprimir: (a: Int, lista: List[Int])Unit""");$skip(703); 
  

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
};System.out.println("""mover: (n: Int, posicion: Int, movimiento: Int, tablero: List[Int])List[Int]""");$skip(26); val res$0 = 


mover(0,62,8,tablero);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(380); ;


    
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
  };System.out.println("""leerColumna: (l: List[Int], i: Int, a: Int, long: Int, l2: List[Int])List[Int]""");$skip(24); val res$1 = 

mover(0,62,8,tablero);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(602); ;

    
	
	
  
  
  
  
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

};System.out.println("""cambio: (posicion: Int, movimiento: String, tablero: List[Int])List[Int]""");$skip(40); 

var cambiado = cambio(12,"d",tablero);System.out.println("""cambiado  : List[Int] = """ + $show(cambiado ));$skip(24); ;

imprimir(0,cambiado);$skip(295); ;


    
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
  };System.out.println("""leerFila: (l: List[Int], i: Int, a: Int, f: Int, l2: List[Int])List[Int]""");$skip(47); val res$2 = 
  

 
  leerFila(cambiado, 0, 5, 9, List());System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(41); val res$3 = 
  leerColumna(cambiado, 0, 1, 9, List());System.out.println("""res3: List[Int] = """ + $show(res$3));$skip(41); val res$4 = 
  leerColumna(cambiado, 0, 8, 9, List());System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(190); 

  

def fila0s(n:Int , fila:List[Int]): List[Int] ={
	if(n==8){
		List();
	}else if( fila(n) == fila(n+1)){
		List(n) ::: fila0s(n+1,fila);
	}else{
		List(n) ::: fila0s(n+1,fila);
		}
};System.out.println("""fila0s: (n: Int, fila: List[Int])List[Int]""");$skip(177); 

def generar0s(n:Int,lista:List[Int] ): List[Int] = { //Lista de enteros de 1 a n

		if(n==0){
			lista;
			}else{
			lista ::: generar0s(n-1,lista) ::: List(0);
			}
		

};System.out.println("""generar0s: (n: Int, lista: List[Int])List[Int]""");$skip(23); val res$5 = 

generar0s(2,List());System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(132); ;



def length(lista:List[Int],longitud:Int): Int ={
	if(lista == Nil){
		longitud;}
	else{
		length(lista.tail,longitud+1);
	}
};System.out.println("""length: (lista: List[Int], longitud: Int)Int""");$skip(31); 
var l = length(List(1,1,1),0);System.out.println("""l  : Int = """ + $show(l ));$skip(225); ;

def todas(n:Int , fila:List[Int]): Int ={ //Devuelve 1 si todos los numeros de la fila son el mismo, sino devuelve 0
	
	if(n+1==length(fila,0)){
		1 }
	else if(fila(n) != fila(n+1)){
		0 }
	else{
		todas(n+1,fila);
	}

};System.out.println("""todas: (n: Int, fila: List[Int])Int""");$skip(26); val res$6 = 
todas(0,List(1,1,1,2,3));System.out.println("""res6: Int = """ + $show(res$6));$skip(131); ;

def reverse(fila:List[Int]) : List[Int] = {

	if(fila==Nil){
		List();
	}else{
		reverse(fila.tail) ::: List(fila.head);
	}

};System.out.println("""reverse: (fila: List[Int])List[Int]""");$skip(249); 

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
};System.out.println("""seguidos: (n: Int, fila: List[Int])Int""");$skip(27); val res$7 = 

seguidos(0,List(1,1,2));System.out.println("""res7: Int = """ + $show(res$7));$skip(123); ;

def quitarHead(n:Int , fila:List[Int]) : List[Int] = {
	if (n > 0){
		quitarHead(n-1, fila.tail);
	}else{
	fila;
	
	}
};System.out.println("""quitarHead: (n: Int, fila: List[Int])List[Int]""");$skip(29); val res$8 = 

quitarHead(2,List(1,1,2));System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(310); ;

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
	
	
};System.out.println("""filanueva: (n: Int, fila: List[Int])List[Int]""");$skip(46); val res$9 = 

filanueva(0,List( 6, 2, 3, 1, 6, 1, 1, 2));System.out.println("""res9: List[Int] = """ + $show(res$9));$skip(303); ;

def colnueva(n:Int , fila:List[Int]) : List[Int] = {
	

	if(n==6){
		List(fila.head);
	}else{
		var seg = seguidos(0,fila);
		if (seg >2) {
			var filaux = quitarHead(seg,fila);
			generar0s(seg,List()) ::: colnueva(n+seg,filaux);
		}else{
			List(fila.head) ::: colnueva(n+1,fila.tail);
		}
	}
	
	
};System.out.println("""colnueva: (n: Int, fila: List[Int])List[Int]""");$skip(34); val res$10 = 

colnueva(0,List(6,5,6,2,2,2,2));System.out.println("""res10: List[Int] = """ + $show(res$10));$skip(259); ;


//Funcion que transforma 7 fila en una matriz
def matrizFilas(l1: List[Int], l2: List[Int], l3: List[Int], l4: List[Int], l5: List[Int], l6: List[Int], l7: List[Int]): List[Int] = {
	val matriz = List() ++ l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6 ++ l7
	matriz
};System.out.println("""matrizFilas: (l1: List[Int], l2: List[Int], l3: List[Int], l4: List[Int], l5: List[Int], l6: List[Int], l7: List[Int])List[Int]""");$skip(757); 

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
};System.out.println("""matrizColumnas: (c1: List[Int], c2: List[Int], c3: List[Int], c4: List[Int], c5: List[Int], c6: List[Int], c7: List[Int], c8: List[Int], c9: List[Int], matriz: List[Int])List[Int]""");$skip(442); 
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
};System.out.println("""comparaMatrices: (l1: List[Int], l2: List[Int], r: List[Int])List[Int]""");$skip(56); val res$11 = 

comparaMatrices(List(8,0,7,9), List(8,6,0,9), List());System.out.println("""res11: List[Int] = """ + $show(res$11));$skip(274); 


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
};System.out.println("""removeZeros: (lst: List[Int], acc: List[Int])List[Int]""");$skip(56); 
val sinceros = removeZeros(List(0,1,3,5,3,0,1), List());System.out.println("""sinceros  : List[Int] = """ + $show(sinceros ));$skip(41); val res$12 = 
removeZeros(List(7,7,7,7,7,7,7), List());System.out.println("""res12: List[Int] = """ + $show(res$12));$skip(237); 

def anadirRec(lst: List[Int], n: Int): List[Int] = {
        if(n <= 0) {
            lst
        }else{
        val r = scala.util.Random;
        val color = 1 + r.nextInt(6);
        anadirRec(List(color) ::: lst, n - 1)
			}
    };System.out.println("""anadirRec: (lst: List[Int], n: Int)List[Int]""");$skip(105); 


def anadir(lst: List[Int]): List[Int] = {
    val lt = length(lst,0)
    anadirRec(lst,7 - lt)
		
};System.out.println("""anadir: (lst: List[Int])List[Int]""");$skip(1083); 

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




};System.out.println("""actualizaTablero: (lst: List[Int])List[Int]""");$skip(303); 
imprimir(0, actualizaTablero(matrizFilas(filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)),filanueva(0,List(1,1,1,2,3,4,4,4,7)))));$skip(22); val res$13 = 


anadir(sinceros);;System.out.println("""res13: List[Int] = """ + $show(res$13))}





}
