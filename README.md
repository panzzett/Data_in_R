# Manipulación y Representación de Datos Estadísticos con R

Este repositorio contiene un desarrollo en **lenguaje R** orientada a la **manipulación, análisis estadístico y representación gráfica de datos**, usando datasets integrados.

## 📌 Objetivos

- Representar gráficamente datos estadísticos mediante diagramas y polígonos.
- Calcular frecuencias absolutas, relativas y acumuladas.
- Identificar medidas estadísticas como la moda utilizando librerías especializadas.
- Aplicar técnicas de visualización en R para facilitar la interpretación de datos.

## 🧠 Contenido

El script incluye:

- **Filtrado y representación de estados de por superficie.**
- **Visualización de series de descubrimientos científicos entre un periodo de años** usando:
  - Diagramas de barras
  - Polígonos de frecuencias
  - Histogramas personalizados
- **Análisis estadístico**: ordenación, cálculo de moda, frecuencias acumuladas.
- **Representación de ventas ficticias en España** con `barplot()`, `pie()`, y `dotchart()`.

## 📚 Librerías utilizadas

- `modeest`: para el cálculo de la **moda estadística** con la función `mfv()`
- Datasets internos de R: `state.area`, `state.abb`, `discoveries`, `precip`

## 💻 Lenguaje y entorno

- **Lenguaje:** R
- **Entorno recomendado:** [RStudio](https://posit.co/download/rstudio-desktop/)

## 📊 gráficas generadas

- Diagrama de barras de áreas de estados con `barplot()`
- Polígono de frecuencias relativas con `plot(type = "o")`
- Histograma de descubrimientos con `hist()`
- Gráfico circular con `pie()`
- Diagrama de puntos con `dotchart()`

