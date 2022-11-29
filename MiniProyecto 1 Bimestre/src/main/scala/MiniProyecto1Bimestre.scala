/*  Autores
* Jose David Piedra
* David Ernesto Carrion
* Pablo Costa Torres
* */
@main def main: Unit = {
  println("Detalles de la primera funcion y sus resultados")
  println(MetodoSimpsonSimple(3, 5, x => (-math.pow(x, 2) + 8 * x - 12)))
  println(MetodoSimpsonCompuesta(3, 5, 16, x => (-math.pow(x, 2) + 8 * x - 12)))
  println(MetodoSimpsonExtendida(3, 5, x => (-math.pow(x, 2) + 8 * x - 12)))
  println("---------------------------------------------------------------------")
  println("Detalles de la segunda funcion y sus resultados")
  println(MetodoSimpsonSimple(0, 2, x => (3 * math.pow(x, 2))))
  println(MetodoSimpsonCompuesta(0, 2, 10, x => (3 * math.pow(x, 2))))
  println(MetodoSimpsonExtendida(0, 2, x => (3 * math.pow(x, 2))))
  println("---------------------------------------------------------------------")
  println("Detalles de la tercera funcion y sus resultados")
  println(MetodoSimpsonSimple(-1, 1, x => (x + 2 * math.pow(x, 2) - math.pow(x, 3) + 5 * math.pow(x, 4))))
  println(MetodoSimpsonCompuesta(-1, 1, 20, x => (x + 2 * math.pow(x, 2) - math.pow(x, 3) + 5 * math.pow(x, 4))))
  println(MetodoSimpsonExtendida(-1, 1, x => (x + 2 * math.pow(x, 2) - math.pow(x, 3) + 5 * math.pow(x, 4))))
  println("---------------------------------------------------------------------")
  println("Detalles de la cuarta funcion y sus resultados")
  println(MetodoSimpsonSimple(1, 2, x => ((2 * x + 1) / (math.pow(x, 2) + x))))
  println(MetodoSimpsonCompuesta(1, 2, 14, x => ((2 * x + 1) / (math.pow(x, 2) + x))))
  println(MetodoSimpsonExtendida(1, 2, x => ((2 * x + 1) / (math.pow(x, 2) + x))))
  println("---------------------------------------------------------------------")
  println("Detalles de la Quinta funcion y sus resultados")
  println(MetodoSimpsonSimple(0, 1, x => (math.pow(math.E, x))))
  println(MetodoSimpsonCompuesta(0, 1, 18, x => (math.pow(math.E, x))))
  println(MetodoSimpsonExtendida(0, 1, x => (math.pow(math.E, x))))
  println("---------------------------------------------------------------------")
  println("Detalles de la Sexta funcion y sus resultados")
  println(MetodoSimpsonSimple(2, 3, x => (1 / math.sqrt(x - 1))))
  println(MetodoSimpsonCompuesta(2, 3, 10, x => (1 / math.sqrt(x - 1))))
  println(MetodoSimpsonExtendida(2, 3, x => (1 / math.sqrt(x - 1))))
  println("---------------------------------------------------------------------")
  println("Detalles de la Septima funcion y sus resultados")
  println(MetodoSimpsonSimple(0, 1, x => (1 / (1 + math.pow(x, 2)))))
  println(MetodoSimpsonCompuesta(0, 1, 12, x => (1 / (1 + math.pow(x, 2)))))
  println(MetodoSimpsonExtendida(0, 1, x => (1 / (1 + math.pow(x, 2)))))
}

def MetodoSimpsonSimple(a: Int, b: Int, c: Double => Double) = {
  val intermedio = ((a + b) / 2.0)
  val x1 = c(a)
  val x2 = c(intermedio)
  val x3 = c(b)
  (b - a) * (x1 + 4 * x2 + x3) / 6
}

def MetodoSimpsonCompuesta(a: Double, b: Double, c: Int, d: Double => Double) = {
  val h = ((b - a) / c)
  val x = new Array[Double](c + 1)
  x(0) = a
  val j = (1 to c).map(j => x(j) = (a + h * j))
  val i = (1 to c / 2).map(j => (d(x(2 * j - 2)) + 4 * d(x(2 * j - 1)) + d(x(2 * j)))).sum
  (i * h) / 3
}

def MetodoSimpsonExtendida(a: Int, b: Int, c: Double => Double) = {
  val n = 2 * (b - a)
  val h = (b - a) / n.toDouble
  val j = c(a) + 4 * (1 to (n - 1) by 2).map(j => c(a + j * h)).sum
  val i = c(b) + 2 * (2 to (n - 2) by 2).map(i => c(a + i * h)).sum
  (h / 3) * (i + j)

}