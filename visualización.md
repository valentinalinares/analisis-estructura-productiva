Análisis de Estructura Productiva - Actividad Integradora M2
================
Valentina Linares (Diplomatura PAEEC - UBA)
2025-06-18

## 1. Introducción y Carga de Datos

Este reporte presenta un análisis sobre la base de datos de “Estructura
Productiva” de Fundar. El objetivo es aplicar técnicas de manipulación y
visualización de datos con R para explorar la evolución del empleo en
distintos países y sectores económicos.

Primero, cargamos las librerías necesarias y los datos desde la carpeta
`/bases`, utilizando rutas relativas para garantizar la portabilidad del
análisis.

``` r
# Carga de librerías
library(tidyverse)
library(plotly)
library(htmlwidgets)

# Carga de datos con rutas relativas
ruta_base <- "bases/empleo_sectores_base.csv"
datos <- read_csv(ruta_base)

# Mostramos las primeras filas para verificar la carga
knitr::kable(head(datos))
```

| iso3 | anio | gran_sector | sector_codigo | sector_desc | empleo_miles | share_empleo |
|:---|---:|:---|:---|:---|---:|---:|
| ARG | 1960 | Bienes | A | Agro y pesca | 1537.29425 | 0.2028724 |
| ARG | 1960 | Bienes | B | Petróleo y minería | 24.97376 | 0.0032957 |
| ARG | 1960 | Bienes | C | Industria | 2001.25488 | 0.2640999 |
| ARG | 1960 | Bienes | DE | Electricidad, gas y agua | 166.22526 | 0.0219363 |
| ARG | 1960 | Bienes | F | Construcción | 445.72845 | 0.0588215 |
| ARG | 1960 | Servicios | GI | Comercio, hoteles y restaurantes | 915.03585 | 0.1207547 |

## 2. Procesamiento Complejo: Resumen para Perú

A continuación, se realiza un procesamiento complejo para generar una
tabla resumen del empleo en Perú, mostrando la proporción que cada gran
sector (Bienes y Servicios) representa sobre el total anual.

``` r
datos_procesados_peru <- datos %>%
  filter(iso3 == "PER") %>%
  group_by(anio, gran_sector) %>%
  summarise(
    empleo_total_gran_sector = sum(empleo_miles, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(anio) %>%
  mutate(
    empleo_total_anual = sum(empleo_total_gran_sector),
    proporcion_gran_sector = empleo_total_gran_sector / empleo_total_anual
  ) %>%
  ungroup()

# Mostramos la tabla resumen generada
knitr::kable(head(datos_procesados_peru))
```

| anio | gran_sector | empleo_total_gran_sector | empleo_total_anual | proporcion_gran_sector |
|---:|:---|---:|---:|---:|
| 1960 | Bienes | 2102.735 | 3102.873 | 0.6776734 |
| 1960 | Servicios | 1000.139 | 3102.873 | 0.3223266 |
| 1961 | Bienes | 2125.338 | 3165.760 | 0.6713515 |
| 1961 | Servicios | 1040.422 | 3165.760 | 0.3286485 |
| 1962 | Bienes | 2099.250 | 3237.390 | 0.6484390 |
| 1962 | Servicios | 1138.140 | 3237.390 | 0.3515610 |

## 3. Visualización Simple: Evolución del Empleo en Perú

Se crea un gráfico de líneas estático para observar la evolución del
empleo total por gran sector en Perú a lo largo del tiempo, basado en la
tabla procesada anteriormente.

``` r
grafico_empleo_simple <- ggplot(datos_procesados_peru, aes(x = anio, y = empleo_total_gran_sector, color = gran_sector)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolución del Empleo Total por Gran Sector en Perú",
    x = "Año",
    y = "Empleo Total (en miles)",
    color = "Gran Sector"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Mostramos el gráfico
print(grafico_empleo_simple)
```

![](visualización_files/figure-gfm/viz-simple-1.png)<!-- -->

## 4. Visualización Compleja: Comparativa Internacional del Sector Servicios

Finalmente, se desarrolla una visualización interactiva para comparar la
evolución de la proporción del empleo en el sector “Servicios” entre los
países seleccionados entre 1988 y 2018. Se incluye una línea vertical en
1995 para marcar el auge del acceso a internet como un evento relevante.

``` r
# Definición de parámetros
paises_seleccionados <- c("PER", "ARG", "CHL", "COL", "BRA", "MEX")
anio_inicio <- 1988
anio_fin <- 2018
sector_analizado <- "Servicios"
anio_evento <- 1995
etiqueta_evento <- "Acceso a internet en incremento"

# Punto de control 1: Verificamos los datos originales
# print(paste("Dimensiones iniciales de 'datos':", paste(dim(datos), collapse=" x ")))

# Procesamiento de datos para el gráfico
datos_filtrados <- datos %>%
  filter(iso3 %in% paises_seleccionados, 
         anio >= anio_inicio, 
         anio <= anio_fin)

# Punto de control 2: Verificamos los datos después de filtrar por país y año
# print(paste("Dimensiones después de filtrar por país y año:", paste(dim(datos_filtrados), collapse=" x ")))

datos_procesados_servicios <- datos_filtrados %>%
  group_by(iso3, anio) %>%
  mutate(
    empleo_total_anual = sum(empleo_miles, na.rm = TRUE),
    proporcion_sector = ifelse(empleo_total_anual == 0, 0, empleo_miles / empleo_total_anual)
  ) %>%
  filter(gran_sector == sector_analizado) %>%
  summarise(
    proporcion_servicios_en_pais = sum(proporcion_sector, na.rm = TRUE),
    .groups = 'drop'
  )

# Punto de control 3: Verificamos los datos finales que irán al gráfico
# print(paste("Dimensiones de la tabla final para el gráfico:", paste(dim(datos_procesados_servicios), collapse=" x ")))
# print(head(datos_procesados_servicios))


# Solo intentamos crear el gráfico si tenemos datos
if(nrow(datos_procesados_servicios) > 0) {
  
  # Creación del gráfico
  grafico_servicios_comparativo <- ggplot(datos_procesados_servicios,
                                          aes(x = anio, y = proporcion_servicios_en_pais, color = iso3, group = iso3,
                                              text = paste("País:", iso3, "<br>Año:", anio, "<br>Proporción:", scales::percent(proporcion_servicios_en_pais, accuracy = 0.1)))) +
    geom_line(linewidth = 0.8, alpha = 0.9) +
    geom_vline(xintercept = anio_evento, linetype = "dashed", color = "red", linewidth = 0.7) +
    annotate("text", x = anio_evento + 0.5, y = max(datos_procesados_servicios$proporcion_servicios_en_pais, na.rm = TRUE) * 0.95,
             label = etiqueta_evento, color = "red", size = 3, hjust = 0) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Proporción del Empleo del Sector 'Servicios' en el Empleo Total del País",
      subtitle = paste("Años:", anio_inicio, "-", anio_fin),
      x = "Año", y = "Proporción del Empleo en Servicios", color = "País (ISO3)"
    ) +
    theme_minimal() +
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

  # Convertir a interactivo y mostrarlo
  print(grafico_servicios_comparativo)

} else {
  
  # Si no hay datos, muestra un mensaje claro
  print("No se encontraron datos para los países y el rango de años especificados, por lo que no se puede generar el gráfico complejo.")
  
}
```

![](visualización_files/figure-gfm/viz-compleja-1.png)<!-- -->
