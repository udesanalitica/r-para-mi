# R para Microbiología Industrial

**Análisis de datos y diseño experimental con un enfoque práctico**

Libro digital de acceso gratuito para la enseñanza del diseño de experimentos y el análisis estadístico aplicados a la microbiología industrial, con el lenguaje de programación R.

📖 **Leer el libro:** <https://udesanalitica.github.io/r-para-mi/>

---

## De qué se trata

Los conceptos estadísticos suelen enseñarse separados de los problemas experimentales que les dan sentido. Este libro parte del problema contrario: cada técnica se introduce a partir de un experimento real de microbiología industrial, con sus datos, su código y su interpretación.

Los ejemplos provienen de trabajos de grado y proyectos académicos desarrollados en la Universidad de Santander (UDES). No son conjuntos de datos genéricos: son experimentos que los estudiantes reconocen porque pertenecen a su propio campo.

**Dirigido a:** estudiantes de pregrado y posgrado en microbiología industrial y áreas afines, y a docentes que enseñen diseño experimental en contextos aplicados.

**Requisitos previos:** ninguno en programación. El libro introduce R desde el comienzo.

---

## Cómo usarlo

El libro se lee en línea sin instalar nada. Para reproducir los análisis en su propio computador se necesita:

- [R](https://cran.r-project.org/) (versión 4.0 o superior)
- [RStudio](https://posit.co/download/rstudio-desktop/) u otro entorno de trabajo
- Los paquetes indicados al inicio de cada capítulo

Todos los ejemplos son reproducibles: el código que genera cada resultado está a la vista y los datos están disponibles en el repositorio.

---

## Compilar el libro localmente

```bash
git clone https://github.com/udesanalitica/r-para-mi.git
cd r-para-mi
quarto render
```

Requiere [Quarto](https://quarto.org/docs/get-started/) instalado. El sitio se genera en la carpeta de salida configurada en `_quarto.yml`.

---

## Estructura del repositorio

```
├── _quarto.yml          Configuración del libro
├── index.qmd            Prefacio
├── Chapter_*.qmd        Capítulos
├── data/                Conjuntos de datos de los ejemplos
├── images/              Figuras
├── CONTRIBUTING.md      Cómo contribuir
└── README.md            Este archivo
```

---

## Cómo contribuir

Las contribuciones son bienvenidas: correcciones, ejercicios adicionales, conjuntos de datos o adaptaciones a otras disciplinas.

- **Reportar un error o sugerir una mejora:** [abrir un *issue*](https://github.com/udesanalitica/r-para-mi/issues)
- **Proponer un cambio concreto:** enviar un *pull request*

Antes de contribuir, consulte [CONTRIBUTING.md](CONTRIBUTING.md). Contiene los requisitos técnicos y, en particular, las reglas sobre aportes de datos e imágenes de terceros.

---

## Licencia

Esta obra es de **acceso gratuito**. La titularidad de los derechos patrimoniales corresponde a la Universidad de Santander (UDES).

> ⚠️ **Licencia en trámite.** Se prevé publicar el texto y las figuras bajo [Creative Commons Atribución-CompartirIgual 4.0 Internacional (CC BY-SA 4.0)](https://creativecommons.org/licenses/by-sa/4.0/deed.es) y el código fuente bajo [licencia MIT](https://opensource.org/licenses/MIT). Mientras la licencia se formaliza ante las instancias institucionales, la reutilización y adaptación del contenido requiere autorización previa de la Universidad de Santander.

Las figuras y los conjuntos de datos aportados por terceros se acreditan individualmente y se reproducen con autorización de sus autores.

---

## Cómo citar

> Ortiz, F. A., Pérez, M. O., y León, F. J. (2025). *R para microbiología industrial: análisis de datos y diseño experimental con un enfoque práctico*. Universidad de Santander, Vicerrectoría de Enseñanza. <https://udesanalitica.github.io/r-para-mi/>

El archivo [`CITATION.cff`](CITATION.cff) permite generar la cita en otros formatos.

---

## Autores

- **Fredy Alejandro Ortiz Meneses** — Microbiología General y Microbiología II
- **Miguel Oswaldo Pérez Pulido** — Proyecto II, Microbiología Industrial · Maestría en Estadística Aplicada y Analítica de Datos
- **Francisco Javier León** — Proyecto I, Microbiología Industrial · Maestría en Estadística Aplicada y Analítica de Datos

Universidad de Santander (UDES) — Bucaramanga, Colombia

Material producido en el marco de la Convocatoria Interna de Producción de Material Profesoral 2025 de la Vicerrectoría de Enseñanza.

---

## Construido con

[Quarto](https://quarto.org) · [R](https://www.r-project.org/) · Publicado con GitHub Pages
