# Cómo contribuir

Gracias por su interés en este proyecto. *R para microbiología industrial* es un material educativo vivo: se corrige, se amplía y se adapta con el tiempo. Las contribuciones de la comunidad son bienvenidas.

Este documento explica qué tipo de aportes se buscan, cómo enviarlos y bajo qué condiciones se incorporan.

---

## Qué se puede aportar

| Tipo | Ejemplos |
|---|---|
| **Correcciones** | Errores de código, erratas, enlaces rotos, resultados que no se reproducen |
| **Mejoras pedagógicas** | Explicaciones más claras, ejercicios adicionales, ejemplos alternativos |
| **Conjuntos de datos** | Datos experimentales propios que ilustren un diseño no cubierto |
| **Nuevos contenidos** | Capítulos o secciones sobre diseños o técnicas no incluidos |
| **Adaptaciones** | Traducciones, versiones para otras disciplinas |

---

## Reportar un problema

Abra un *issue* en <https://github.com/udesanalitica/r-para-mi/issues> e incluya:

1. El capítulo y la sección donde ocurre.
2. Qué esperaba y qué obtuvo.
3. Si es un error de código: el mensaje completo y el resultado de `sessionInfo()`.

Cada página del libro tiene un enlace directo para reportar problemas y otro para editarla.

---

## Proponer un cambio

1. Haga un *fork* del repositorio.
2. Cree una rama descriptiva: `git checkout -b correccion-anova-capitulo-8`.
3. Realice los cambios y verifique que el libro compile con `quarto render`.
4. Envíe un *pull request* explicando qué cambia y por qué.

Para cambios grandes —un capítulo nuevo, una reorganización— conviene abrir primero un *issue* para conversarlo, y así evitar trabajo que después no encaje con la estructura del material.

---

## Requisitos técnicos

- **Reproducibilidad.** Todo código debe ejecutarse de principio a fin sin intervención manual. Si el análisis usa aleatorización, fije la semilla con `set.seed()`.
- **Datos accesibles.** Los datos deben estar en el repositorio o descargarse desde el propio código. No se admiten rutas locales.
- **Estilo.** Siga las convenciones del [tidyverse style guide](https://style.tidyverse.org/): nombres en minúscula con guion bajo, `<-` para asignación, líneas de máximo 80 caracteres.
- **Idioma.** El contenido se escribe en español. Los nombres de objetos y variables también, siempre que sean legibles.
- **Dependencias.** Si el aporte requiere un paquete nuevo, indíquelo en el *pull request* y justifique por qué es necesario.

---

## Contribuciones de datos e imágenes

Esta sección es importante y no es negociable.

**Datos experimentales.** Solo se aceptan datos sobre los que quien contribuye tenga derecho a decidir. Si provienen de un trabajo de grado, una tesis o un proyecto de investigación de otra persona, se requiere su autorización escrita antes de proponerlos, y así debe indicarse en el *pull request*.

**Datos personales.** No se aceptan conjuntos de datos que contengan información personal identificable, ni datos sujetos a acuerdos de confidencialidad.

**Figuras e imágenes.** No se admiten figuras, diagramas, fotografías ni capturas tomadas de libros, artículos o sitios web, aunque estén citados correctamente. Citar la fuente evita el plagio, pero no otorga permiso de reproducción, y este material se publica bajo una licencia que autoriza a terceros a reutilizarlo.

Para ilustrar una idea que ya aparece en otra obra:

- Constrúyala desde cero, con estructura y redacción propias. Las ideas no están protegidas; la expresión concreta sí.
- Prefiera diagramas generados por código —Mermaid, DiagrammeR o Graphviz— en lugar de imágenes insertadas. Quedan editables, versionables y reproducibles.
- Cite la fuente conceptual en el texto, no como origen de la figura. Evite el rótulo "adaptado de": una adaptación es una obra derivada y también requiere autorización.

**Fotografías de laboratorio.** Deben ser propias o contar con autorización de quien las tomó. Si aparecen personas identificables, se requiere su consentimiento expreso.

---

## Licenciamiento de las contribuciones

Al enviar una contribución, quien la aporta acepta que se publique bajo las mismas licencias del proyecto: **CC BY-SA 4.0** para el texto y las figuras, y **licencia MIT** para el código.

> **Estado del licenciamiento.** Estas licencias se encuentran en trámite de formalización ante las instancias de la Universidad de Santander, titular de los derechos patrimoniales de la obra. Los aportes se publicarán bajo ellas una vez formalizadas; hasta ese momento se conservan bajo las mismas condiciones que el resto del material, es decir, de acceso gratuito y con reutilización sujeta a autorización previa de la Universidad.

Quien contribuye conserva en todo caso la autoría de su aporte: los derechos morales de paternidad e integridad son irrenunciables.

---

## Reconocimiento

Quienes contribuyan de forma sustantiva se reconocen en la sección **Datos y material experimental** o en la sección de contribuciones del prefacio, según corresponda. Los aportes de datos e imágenes se acreditan además en el pie de la figura o del conjunto de datos respectivo.

Si un aporte alcanza el volumen de un capítulo, la coautoría se discute caso por caso.

---

## Convivencia

Este es un proyecto educativo y el trato entre participantes debe ser respetuoso. Las discusiones técnicas son bienvenidas; los ataques personales no. Quien mantenga el proyecto puede cerrar o moderar cualquier intercambio que se aparte de esto.

---

## Contacto

Para dudas que no encajen en un *issue*: [correo de contacto del proyecto]

Universidad de Santander (UDES) — Bucaramanga, Colombia