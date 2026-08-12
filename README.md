# R para Mí

**Diseño de experimentos con R aplicado a la microbiología industrial**

Recurso educativo digital de acceso gratuito que articula problemas experimentales de microbiología industrial, datos, código R e interpretación estadística.

📖 **Libro:** <https://udesanalitica.github.io/r-para-mi/>

💻 **Repositorio:** <https://github.com/udesanalitica/r-para-mi>

## Diferencial del proyecto

El recurso no presenta el diseño de experimentos de forma genérica. Los casos se construyen a partir de problemas, datos y materiales vinculados con trabajos de grado y experiencias académicas de la Universidad de Santander (UDES), permitiendo conectar el razonamiento estadístico con situaciones propias de microbiología industrial y biotecnología.

La secuencia de aprendizaje propuesta es:

**problema microbiológico → diseño experimental → datos → código R → análisis → interpretación científica**

## Contenidos

El libro incluye introducción a R y RStudio, bibliometría aplicada, fundamentos de diseño experimental, DCA, DBCA, ANOVA, diagnóstico de supuestos, comparaciones múltiples, visualización e introducción al uso de inteligencia artificial para simulación de datos.

## Reproducibilidad

El proyecto está construido con R y Quarto. Para reproducir los análisis se requiere R, Quarto y los paquetes indicados en los capítulos.

``` bash
git clone https://github.com/udesanalitica/r-para-mi.git
cd r-para-mi
quarto render
```

Los ejemplos deben utilizar rutas relativas al proyecto. No deben incorporarse rutas locales como `C:/...` o `D:/...`.

## Uso educativo y evidencia

En la postulación a LatinR 2026 se reportó el uso del recurso como material complementario en clases de la Maestría en Biotecnología de la UDES. En la versión revisada del sitio no se presentan afirmaciones cuantitativas de aceptación o impacto mientras no exista una evaluación sistemática documentada.

La página `experiencia-uso.qmd` describe el estado de la evidencia y el plan de evaluación previsto.

## Estructura recomendada del repositorio

``` text
├── _quarto.yml
├── index.qmd
├── intro.qmd
├── autores.qmd
├── agradecimientos.qmd
├── Chapter_01.qmd
├── Chapter_02.qmd
├── Chapter_03.qmd
├── Chapter_04.qmd
├── Chapter_05.qmd
├── Chapter_06.qmd
├── experiencia-uso.qmd
├── desarrollo-recurso.qmd
├── contribuir.qmd
├── licencia.qmd
├── data/
├── images/
├── docs/
├── references.bib
├── references.qmd
├── CONTRIBUTING.md
├── LICENSE.md
├── CITATION.cff
└── README.md
```

No se recomienda versionar `.RData`, `.Rhistory`, `.Rproj.user/`, `.quarto/`, `_book/` ni `_freeze/`.

## Cómo contribuir

Las contribuciones son bienvenidas mediante GitHub Issues y Pull Requests. Consulte [CONTRIBUTING.md](CONTRIBUTING.md) antes de proponer cambios, especialmente si incluyen datos o imágenes de terceros.

## Derechos y licenciamiento

La obra es de acceso gratuito y la titularidad patrimonial corresponde a la Universidad de Santander (UDES).

> **Licenciamiento en formalización.** Se proyecta utilizar CC BY-SA 4.0 para el texto y las figuras propias y MIT para el código fuente. Hasta la formalización institucional, la reutilización y adaptación del contenido debe sujetarse a la autorización correspondiente de la Universidad.

## Cómo citar

> Ortiz, F. A., Pérez, M. O., y León, F. J. (2025). *R para microbiología industrial: análisis de datos y diseño experimental con un enfoque práctico*. Universidad de Santander, Vicerrectoría de Enseñanza. <https://udesanalitica.github.io/r-para-mi/>

## Autores

-   **Fredy Alejandro Ortiz Meneses** — Microbiología General y Microbiología II
-   **Miguel Oswaldo Pérez Pulido** — Proyecto II, Microbiología Industrial · Maestría en Estadística Aplicada y Analítica de Datos
-   **Francisco Javier León** — Proyecto I, Microbiología Industrial · Maestría en Estadística Aplicada y Analítica de Datos

Universidad de Santander (UDES) — Bucaramanga, Colombia

Material producido en el marco de la Convocatoria Interna de Producción de Material Profesoral 2025 de la Vicerrectoría de Enseñanza.

## Construido con

R · Quarto · GitHub · GitHub Pages
