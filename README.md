![](img/interculturales2.png)

# Seminario Cuantitativo

# ***InteRculturales:** Técnicas de ciencias sociales computacionales desde la relaciones interculturales.*

Público objetivo: Cualquier persona interesada en aprender o interiorizarse en habilidades cuantitativas de vanguardia.

**Inicio de curso: Jueves 8 de mayo 2025. Desde las 18:00**

El objetivo del Seminario es integrar las técnicas de Ciencias Sociales Computacionales a las relaciones interculturales. Este seminario busca: En primer lugar, entregar herramientas básicas para que sus asistentes pueden desenvolverse con éxito en trabajos que requieran habilidades cuantitativas. En segundo lugar, que se establezca una comunidad de práctica que reflexione y desafíe sobre las implicancias de las técnicas cuantitativas y la emergencia de la Inteligencia Artificial en nuestras (actuales o futuras) profesiones.

El curso se centrará en el trabajo con datos de encuestas, orientados a la gestión y comunicación de información; datos geográficos, que permitirán georreferenciar poblaciones e instituciones; e información textual, que abrirá posibilidades de análisis de contenidos. Finalmente, se explorarán técnicas emergentes de gestión de la información, como el Web Scraping y el uso de Inteligencia Artificial. Se utilizará el lenguaje de programación R y Python.

A lo largo del Seminario, los participantes aprenderán a aplicar metodologías innovadoras para procesar, analizar y comunicar resultados, integrando visualizaciones avanzada que resalten las interacciones entre diferentes grupos étnicos, los sesgos presentes en los datos y las complejidades de las relaciones interculturales en sus diversos contextos.

Inscripciones aquí: [Aquí](https://forms.cloud.microsoft/r/10cuEygf00)

# Temario del curso

Antes de iniciar el curso, se solicita que todas tengan el software R instalado. Su descarga se puede realizar desde el siguiente link [R](https://cran.r-project.org/bin/windows/base/) y en posterioridad descargar la interfaz de [R studio](https://posit.co/download/rstudio-desktop/). Un tutorial muy explicativo se encuentra aquí: [Tutorial de instalación](https://www.youtube.com/watch?v=RtkCAKXsVbw&t=204s). Otra excelente opción: [Tutorial Instalación II](https://bastianolea.rbind.io/blog/r_introduccion/instalar_r/). En caso de no poder instalar R en su computador existe la versión de la nube de Posit Cloud. Un vídeo explicativo se puede encontrar aquí: [Tutorial Posit Cloud](https://www.youtube.com/watch?v=hZuCmgoSGzM). También puedes ingresar directamente por aquí: [Posit Cloud](https://posit.cloud/).

Se solicita considerar este vídeo: [Rutas de trabajo](https://www.youtube.com/watch?v=gWcmdA_uGVY). Esto es una cuestión ESENCIAL para el trabajo en R. Durante las sesiones no nos detendremos en enseñarlo.

La grabación de todas las sesiones pueden ser encontradas aquí: [Clases InteRculturales (YouTube)](https://youtube.com/playlist?list=PL8V8dGNnJoBQUQ0lXLNRAtuDXb3UCZ9NX&si=Q6AmGrO4gpyabDlW)

# Temario

| **Sesión** | **Contenido**                                 | **Presentación**                                                                                                     | **Código práctico**                                                                                                                                                                                                                                                                                                                                                                                | **Paquetes fundamentales**      | **Referencias**                                                                                                                                             |
|-----------|-----------|-----------|-----------------|-----------|-----------|
| 08/05/2025 | Procesamiento de Datos en R con Tidyverse     | [Presentación 1](https://centrociir.github.io/interculturales/clases/clase1/pres/presentacion-1.html)                | [Manual Práctico](https://centrociir.github.io/interculturales/clases/clase1/clase_1.html) <br> [Posit Cloud](https://posit.cloud/content/10310196) <br> [Código práctico](https://github.com/centrociir/interculturales/blob/main/clases/clase1/practico/practico_1.R) <br> [Clase 1 (PDF)](https://github.com/centrociir/interculturales/blob/main/clases/clase1/interculturales_1.pdf)          | `tidyverse`                     | [Introducción a Tidyverse](https://r4ds.had.co.nz/tidy-data.html)                                                                                           |
| 15/05/2025 | Visualización de Datos con ggplot2            | [Presentación 2](https://centrociir.github.io/interculturales/clases/clase2/presentacion/presentacion-2.html)        | [Manual Práctico 2](https://centrociir.github.io/interculturales/clases/clase2/practico/practico2.html) <br> [Posit Cloud](https://posit.cloud/content/10365718) <br> [Script práctico](https://github.com/centrociir/interculturales/blob/main/clases/clase2/script_practico.R) <br> [Clase 2 (PDF)](https://github.com/centrociir/interculturales/blob/main/clases/clase2/interculturales_2.pdf) | `ggplot2`, `tidyverse`          | [R4DS: Visualización](https://r4ds.had.co.nz/data-visualisation.html)                                                                                       |
| 22/05/2025 | Análisis Geográficos: Creación y uso de Mapas | [Presentación 3](https://centrociir.github.io/interculturales/clases/clase3/presentacion/presentacion_clase3.html$%) | [Script práctico](https://github.com/centrociir/interculturales/blob/main/clases/clase3/practico/codigo_practico3.R%$)                                                                                                                                                                                                                                                                             | `chilemapas`, `sf`, `tidyverse` | [ChileMapas](https://github.com/juanmiguelsr/chilemapas) <br> [Tutorial Bastián Olea](https://rpubs.com/bastimapache/mapa_urbano_rm)                        |
|            |                                               |                                                                                                                      | **RECESO**                                                                                                                                                                                                                                                                                                                                                                                         |                                 |                                                                                                                                                             |
| 05/06/2025 | Análisis Cuantitativo de Texto en R           |                                                                                                                      |                                                                                                                                                                                                                                                                                                                                                                                                    | `quanteda`, `tidyverse`         | [Text Mining with R](https://www.tidytextmining.com) <br> [Paper 1](https://doi.org/10.1080/19312458.2017.1387238) <br> [quanteda.io](https://quanteda.io/) |
| 12/06/2025 | Uso de APIs en R                              |                                                                                                                      |                                                                                                                                                                                                                                                                                                                                                                                                    |                                 | [OpenAI Docs](https://platform.openai.com/docs/)                                                                                                            |
| 19/06/2025 | Web Scraping en R                             |                                                                                                                      |                                                                                                                                                                                                                                                                                                                                                                                                    |                                 |                                                                                                                                                             |

![](img/sticker_ciir_rosa_v2.png)

## Follow en GitHub...

Personas chilenas que tienen un contenido de R y Pyhon interesante:

-   [\@bastianolea](https://github.com/bastianolea)

-   [\@rivaquiroga](https://github.com/rivaquiroga/)

-   [\@pachadotdev](https://github.com/pachadotdev)

-   [\@robsalasco](https://github.com/robsalasco)

-   [\@mebucca](https://github.com/mebucca)

Personas a lo largo del mundo.

-   [\@juliasilge](https://github.com/)

-   [\@hadley](https://github.com/hadley)

-   [\@yihui](https://github.com/yihui)
