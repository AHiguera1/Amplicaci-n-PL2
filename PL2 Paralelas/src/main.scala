

object main {
  def main(args:Array[String]) { 
    
    //Declaramos objetos
    val aux = new Auxiliares;
    val juego = new JuegoBasico;
    
    
    //Generamos el tablero
    val tablero = juego.generarTablero(7*9, List());   
    aux.imprimir(0, tablero)
    
    //bucle
    
    juego.turno(2,tablero)
    
    println("sacabo")
    
  }

}