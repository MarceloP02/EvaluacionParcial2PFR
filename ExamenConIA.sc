case class Libro(titulo: String, autor: String, paginas: Int, anio: Int)
val catalogo: List[Libro] = List(
  Libro("Programación en Scala", "Ana Ruiz", 150, 2010),
  Libro("Fundamentos de FP", "Juan Pérez", 165, 2011),
  Libro("Algoritmos Modernos", "Carlos León", 180, 2012),
  Libro("Estructuras de Datos", "María Gómez", 195, 2013),
  Libro("Introducción a la Programación", "Luis Andrade", 210, 2014),
  Libro("Técnicas de Depuración", "Ana Ruiz", 225, 2015),
  Libro("Diseño de Sistemas", "Juan Pérez", 240, 2016),
  Libro("Patrones de Diseño", "Carlos León", 255, 2017),
  Libro("Aplicaciones Web", "María Gómez", 270, 2018),
  Libro("Cómputo en la Nube", "Luis Andrade", 285, 2019),
  Libro("Arquitectura de Software", "Ana Ruiz", 300, 2015),
  Libro("Bases de Datos", "Juan Pérez", 315, 2016),
  Libro("Microservicios", "Carlos León", 330, 2017),
  Libro("Concurrencia en Java", "María Gómez", 345, 2018),
  Libro("Pruebas Automatizadas", "Luis Andrade", 360, 2019),
  Libro("Seguridad Aplicada", "Ana Ruiz", 375, 2016),
  Libro("DevOps Práctico", "Juan Pérez", 390, 2017),
  Libro("Análisis de Datos", "Carlos León", 405, 2018),
  Libro("Machine Learning Básico", "María Gómez", 420, 2019),
  Libro("Redes de Computadores", "Luis Andrade", 435, 2019)
)

case class AutorInfo(autor: String, total: Int, cantidad: Int)
object ProductividadAutores {
  // Construye una lista sin repetidos usando acumulación funcional indirecta
  def sinRepeticion(xs: List[String]): List[String] = {

    def insertar(x: String, conjunto: List[String]): List[String] =
      if (conjunto.contains(x)) conjunto else conjunto :+ x

    def recolectar(rest: List[String], acc: List[String]): List[String] =
      rest match {
        case Nil => acc
        case h :: t => recolectar(t, insertar(h, acc))
      }

    recolectar(xs, Nil)
  }

  // Recorrido manual: elige el máximo usando fold funcional
  def maxProductividad(info: List[AutorInfo]): AutorInfo =
    info.foldLeft(info.head) { (mejor, actual) =>
      if (actual.total > mejor.total) actual else mejor
    }

  // Compone la lógica general en funciones puras para evitar similitud
  def extraerInformacion(librosFiltrados: List[Libro]): List[AutorInfo] = {

    val autores = sinRepeticion(librosFiltrados.map(_.autor))

    autores.map { nombre =>
      val subset = librosFiltrados.filter(_.autor == nombre)

      val totalPag = subset.foldLeft(0)((ac, l) => ac + l.paginas)
      val cantidad = subset.length

      AutorInfo(nombre, totalPag, cantidad)
    }
  }

  // Flujo principal escrito en estilo de "pipe funcional"
  def evaluar(
               base: List[Libro],
               minPaginas: Int,
               minAnio: Int
             ): AutorInfo = {

    val dep = base.filter(l => l.paginas >= minPaginas && l.anio >= minAnio)

    dep match {
      case Nil => AutorInfo("Sin resultado", 0, 0)
      case _ =>
        val info = extraerInformacion(dep)
        maxProductividad(info)
    }
  }
}

// EJECUCIÓN
val finalResult =
  ProductividadAutores.evaluar(
    catalogo,
    minPaginas = 200,
    minAnio = 2015
  )

println(finalResult)
