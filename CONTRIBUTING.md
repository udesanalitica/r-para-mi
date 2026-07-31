# Cómo contribuir

Gracias por tu interés en este proyecto. *R para microbiología industrial* es un material educativo vivo: se corrige, se amplía y se adapta con el tiempo. Las contribuciones de la comunidad son bienvenidas.

Este documento explica qué tipo de aportes buscamos, cómo enviarlos y bajo qué condiciones se incorporan.

---

## Qué puedes aportar

| Tipo | Ejemplos |
|---|---|
| **Correcciones** | Errores de código, erratas, enlaces rotos, resultados que no se reproducen |
| **Mejoras pedagógicas** | Explicaciones más claras, ejercicios adicionales, ejemplos alternativos |
| **Conjuntos de datos** | Datos experimentales propios que ilustren un diseño no cubierto |
| **Nuevos contenidos** | Capítulos o secciones sobre diseños o técnicas no incluidos |
| **Adaptaciones** | Traducciones, versiones para otras disciplinas |

---

## Reportar un problema

Abre un *issue* en <https://github.com/udesanalitica/r-para-mi/issues> e incluye:

1. El capítulo y la sección donde ocurre.
2. Qué esperabas y qué obtuviste.
3. Si es un error de código: el mensaje completo y el resultado de `sessionInfo()`.

Cada página del libro tiene un enlace directo para reportar problemas y otro para editarla.

---

## Proponer un cambio

1. Haz un *fork* del repositorio.
2. Crea una rama descriptiva: `git checkout -b correccion-anova-capitulo-8`.
3. Haz los cambios y verifica que el libro compile con `quarto render`.
4. Envía un *pull request* explicando qué cambia y por qué.

Para cambios grandes —un capítulo nuevo, una reorganización— abre primero un *issue* para conversarlo. Evita trabajo que después no encaje con la estructura del material.

---

## Requisitos técnicos

- **Reproducibilidad.** Todo código debe ejecutarse de principio a fin sin intervención manual. Si el análisis usa aleatorización, fija la semilla con `set.seed()`.
- **Datos accesibles.** Los datos deben estar en el repositorio o descargarse desde el propio código. Nada de rutas locales.
- **Estilo.** Sigue las convenciones del [tidyverse style guide](https://style.tidyverse.org/): nombres en minúscula con guion bajo, `<-` para asignación, líneas de máximo 80 caracteres.
- **Idioma.** El contenido se escribe en español. Los nombres de objetos y variables también, siempre que sean legibles.
- **Dependencias.** Si tu aporte requiere un paquete nuevo, indícalo en el *pull request* y justifica por qué es necesario.

---

## Contribuciones de datos e imágenes

Esta sección es importante y no es negociable.

**Datos experimentales.** Solo se aceptan datos sobre los que tengas derecho a decidir. Si provienen de un trabajo de grado, una tesis o un proyecto de investigación de otra persona, necesitas su autorización escrita antes de proponerlos. Indícalo en el *pull request*.

**Datos personales.** No se aceptan conjuntos de datos que contengan información personal identificable, ni datos sujetos a acuerdos de confidencialidad.

**Figuras e imágenes.** No envíes figuras, diagramas, fotografías ni capturas tomadas de libros, artículos o sitios web, aunque estén citados correctamente. Citar la fuente evita el plagio, pero no otorga permiso de reproducción, y este material se publica bajo una licencia que autoriza a terceros a reutilizarlo.

Si necesitas ilustrar una idea que ya aparece en otra obra:

- Constrúyela desde cero con tu propia estructura y redacción. Las ideas no están protegidas; la expresión concreta sí.
- Prefiere diagramas generados por código —Mermaid, DiagrammeR o Graphviz— en lugar de imágenes insertadas. Quedan editables, versionables y reproducibles.
- Cita la fuente conceptual en el texto, no como origen de la figura. Evita el rótulo "adaptado de": una adaptación es una obra derivada y también requiere autorización.

**Fotografías de laboratorio.** Deben ser tuyas o contar con autorización de quien las tomó. Si aparecen personas identificables, se requiere su consentimiento expreso.

---

## Licenciamiento de las contribuciones

Al enviar una contribución aceptas que se publique bajo las mismas condiciones que el resto del material.

> **Nota sobre el estado del licenciamiento.** La licencia abierta definitiva de esta obra se encuentra en trámite ante las instancias de la Universidad de Santander, titular de los derechos patrimoniales. Se prevé publicarla bajo Creative Commons Atribución-CompartirIgual 4.0 Internacional (CC BY-SA 4.0) para el texto y las figuras, y bajo licencia MIT para el código fuente. Al contribuir aceptas que tu aporte se publique bajo esas licencias una vez formalizadas.

Conservas en todo caso la autoría de tu aporte: los derechos morales de paternidad e integridad son irrenunciables.

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
