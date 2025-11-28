case class Libro(titulo: String, autor: String, paginas: Int, anio: Int)
case class AutorInfo(autor: String, totalPaginas: Int, cantidadLibros: Int)

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

def autoresSinRepetir(libros: List[Libro]): List[String] = {
  val autoresTodos: List[String] = libros.map((l: Libro) => l.autor)

  def aux(pos: Int, acumulados: List[String]): List[String] = {
    val n: Int = autoresTodos.size
    if (pos >= n) {
      acumulados
    } else {
      val actual: String = autoresTodos(pos)
      val yaEsta: Boolean = acumulados.contains(actual)
      val nuevoAcumulados: List[String] =
        if (yaEsta) acumulados else acumulados :+ actual
      aux(pos + 1, nuevoAcumulados)
    }
  }

  aux(0, List())
}


def mejorAutorInfo(lista: List[AutorInfo]): AutorInfo = {
  def aux(pos: Int, mejor: AutorInfo): AutorInfo = {
    val n: Int = lista.size
    if (pos >= n) {
      mejor
    } else {
      val actual: AutorInfo = lista(pos)
      val nuevoMejor: AutorInfo =
        if (actual.totalPaginas > mejor.totalPaginas) actual else mejor
      aux(pos + 1, nuevoMejor)
    }
  }

  aux(1, lista(0))
}

def autorMasProductivo(
                        libros: List[Libro],
                        paginasMin: Int,
                        anioMin: Int
                      ): AutorInfo = {

  val librosFiltrados: List[Libro] =
    libros.filter((l: Libro) => l.paginas >= paginasMin && l.anio >= anioMin)

  if (librosFiltrados.size == 0) {
    AutorInfo("Sin resultado", 0, 0)
  } else {

    val autores: List[String] = autoresSinRepetir(librosFiltrados)

    val listaInfo: List[AutorInfo] =
      autores.map((nombreAutor: String) => {
        val librosAutor: List[Libro] =
          librosFiltrados.filter((l: Libro) => l.autor == nombreAutor)

        val totalPaginas: Int =
          librosAutor.map((l: Libro) => l.paginas).sum

        val cantidadLibros: Int =
          librosAutor.size

        AutorInfo(nombreAutor, totalPaginas, cantidadLibros)
      })

    val autorTop: AutorInfo = mejorAutorInfo(listaInfo)

    autorTop
  }
}

val resultado: AutorInfo =
  autorMasProductivo(catalogo, paginasMin = 200, anioMin = 2015)
