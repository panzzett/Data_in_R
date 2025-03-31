# =======================
# Análisis y Visualización de Datos en R
# =======================

# =======================
# Actividad: Representación de estados con área > 100000
# =======================
estados_filtrados <- state.abb[state.area > 100000]
areas_filtradas <- state.area[state.area > 100000]

barplot(
  areas_filtradas,
  names.arg = estados_filtrados,
  xlab = "Abreviaturas de los Estados",
  ylab = "Área (millas cuadradas)",
  main = "Estados de EE.UU. con Áreas > 100,000 Millas Cuadradas",
  col = rainbow(length(areas_filtradas)),
  border = "black",
  horiz = FALSE,
  cex.names = 0.8,
  cex.axis = 0.8,
  las = 1
)

# =======================
# Actividad: Dataset 'discoveries'
# =======================
data("discoveries")

# Frecuencias
frecuencias_absolutas <- table(discoveries)
frecuencias_relativas <- frecuencias_absolutas / sum(frecuencias_absolutas)
frecuencias_absolutas_acum <- cumsum(frecuencias_absolutas)
frecuencias_relativas_acum <- cumsum(frecuencias_relativas)

# Diagramas de frecuencias
barplot(
  frecuencias_absolutas,
  xlab = "Cantidad de descubrimientos",
  ylab = "Frecuencia absoluta",
  main = "Diagrama de Frecuencias Absolutas",
  col = "lightcoral",
  border = "darkred",
  cex.names = 0.8
)

plot(
  as.numeric(names(frecuencias_absolutas)), frecuencias_absolutas,
  type = "o", col = "navy", pch = 16,
  xlab = "Número de descubrimientos",
  ylab = "Frecuencia absoluta",
  main = "Polígono de Frecuencia Absoluta"
)

barplot(
  frecuencias_relativas,
  xlab = "Cantidad de descubrimientos",
  ylab = "Frecuencia relativa",
  main = "Diagrama de Frecuencias Relativas",
  col = "lightblue",
  border = "blue",
  cex.names = 0.8
)

plot(
  as.numeric(names(frecuencias_relativas)), frecuencias_relativas,
  type = "o", col = "forestgreen", pch = 16,
  xlab = "Número de descubrimientos",
  ylab = "Frecuencia relativa",
  main = "Polígono de Frecuencia Relativa"
)

barplot(
  frecuencias_absolutas_acum,
  xlab = "Cantidad de descubrimientos",
  ylab = "Frecuencia absoluta acumulada",
  main = "Diagrama de Frecuencias Absolutas Acumuladas",
  col = "lightgreen",
  border = "darkgreen",
  cex.names = 0.8
)

plot(
  as.numeric(names(frecuencias_absolutas_acum)), frecuencias_absolutas_acum,
  type = "o", col = "darkorange", pch = 16,
  xlab = "Número de descubrimientos",
  ylab = "Frecuencia absoluta acumulada",
  main = "Polígono de Frecuencia Absoluta Acumulada"
)

barplot(
  frecuencias_relativas_acum,
  xlab = "Cantidad de descubrimientos",
  ylab = "Frecuencia relativa acumulada",
  main = "Diagrama de Frecuencias Relativas Acumuladas",
  col = "lightyellow",
  border = "gold",
  cex.names = 0.8
)

plot(
  as.numeric(names(frecuencias_relativas_acum)), frecuencias_relativas_acum,
  type = "o", col = "orange", pch = 16,
  xlab = "Número de descubrimientos",
  ylab = "Frecuencia relativa acumulada",
  main = "Polígono de Frecuencia Relativa Acumulada"
)

# =======================
# Actividad: Ventas en España (diagramas simples)
# =======================
ventas <- c(345.3, 452.1, 395.6)
names(ventas) <- c("Pedro", "Juan", "Maria")

barplot(ventas, main = "Ventas España", ylab = "Miles de euros")
pie(ventas, main = "Ventas España")
dotchart(ventas, main = "Ventas España", xlab = "Miles de euros")

# =======================
# Actividad: Análisis Estadístico del Dataset 'discoveries'
# =======================

# Ordenar los datos
datos_ordenados <- sort(discoveries)
print(datos_ordenados)

# Instalar y cargar el paquete para calcular la moda
if (!require(modeest)) install.packages("modeest")
library(modeest)

# Calcular la moda con la función mfv (Most Frequent Value)
moda_mfv <- mfv(discoveries)
print(moda_mfv)

# Crear histograma personalizado
hist(
  discoveries,
  main = "Histograma de Discoveries (1860–1959)",
  xlab = "Número de descubrimientos",
  ylab = "Frecuencia",
  col = "skyblue",
  border = "black"
)
