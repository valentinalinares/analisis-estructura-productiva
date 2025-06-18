library(tidyverse) 
library(plotly)
library(htmlwidgets)

# Rutas 
ruta_diccionario <- "bases/diccionario_empleo_sectores_vars.xlsx"
ruta_base <- "bases/empleo_sectores_base.csv"


# -----------------------------------------------------------------------------
# Procesamiento complejo
# -----------------------------------------------------------------------------


datos <- read_csv(ruta_base)

datos_procesados <- datos %>%
  filter(iso3 == "PER") %>% # Filtrar solo datos de Perú
  group_by(anio, gran_sector) %>% # año y gran sector
  summarise(
    empleo_total_gran_sector = sum(empleo_miles, na.rm = TRUE) # empleo total por gran sector
  ) %>%
  ungroup() %>% 
  group_by(anio) %>% # agrupar nuevamente por año
  mutate(
    empleo_total_anual = sum(empleo_total_gran_sector), # empleo total por año
    proporcion_gran_sector = empleo_total_gran_sector / empleo_total_anual # proporción por gran sector
  ) %>%
  ungroup() 

#guardar la tabla en un nuevo csv
write_csv(datos_procesados, "bases/empleo_sectores_procesados.csv")
head(datos_procesados)
View(datos_procesados)

# -----------------------------------------------------------------------------
# Visualización simple: Evolución del Empleo Total por Gran Sector en Perú
# -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

datos_procesados3 <- datos %>%
  filter(iso3 == "PER") %>% 
  group_by(anio, gran_sector) %>%
  summarise(
    empleo_total_gran_sector = sum(empleo_miles, na.rm = TRUE) # sumar el empleo total
  ) %>%
  ungroup()

# gráfico de líneas
grafico_empleo <- ggplot(datos_procesados3, aes(x = anio, y = empleo_total_gran_sector, color = gran_sector)) +
  geom_line(size = 1) + # Agregar líneas
  geom_point(size = 2) + # Agregar puntos
  labs(
    title = "Evolución del Empleo Total por Gran Sector en Perú",
    x = "Año",
    y = "Empleo Total (en miles)",
    color = "Gran Sector"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 12),
    legend.position = "bottom"
  )

print(grafico_empleo)

# guardar en carpeta "resultados"
ggsave("bases/evolucion_empleo_peru.png", 
       plot = grafico_empleo, 
       width = 8, 
       height = 6, 
       dpi = 300)

# -----------------------------------------------------------------------------
# Visualización compleja: Proporción del Empleo del Sector 'Servicios' en el Empleo Total del País
# -----------------------------------------------------------------------------
library(tidyverse)
library(plotly)
library(htmlwidgets)

datos <- read_csv(ruta_base)

paises_seleccionados <- c("PER", "ARG", "CHL", "COL", "BRA", "MEX")
anio_inicio <- 1988
anio_fin <- 2018
sector_analizado <- "Servicios"
anio_evento <- 1995
etiqueta_evento <- "Acceso a Internet en Incremento"

datos_procesados_servicios <- datos %>%
  filter(iso3 %in% paises_seleccionados) %>%         
  filter(anio >= anio_inicio & anio <= anio_fin) %>% 
  filter(gran_sector == sector_analizado) %>%        
  group_by(iso3, anio, gran_sector) %>%              
  summarise(
    empleo_total_gran_sector = sum(empleo_miles, na.rm = TRUE), # empleo total para el sector Servicios
    .groups = 'drop'
  ) %>%
  # se necesita el empleo total ANUAL del PAÍS para calcular la proporción correctamente
  # Por eso: primero calculamos el empleo total por país y año (sumando Bienes y Servicios) y luego lo unimos para calcular la proporción del sector Servicios.
  left_join(
    datos %>%
      filter(iso3 %in% paises_seleccionados) %>%
      filter(anio >= anio_inicio & anio <= anio_fin) %>%
      group_by(iso3, anio) %>%
      summarise(empleo_total_anual_pais_global = sum(empleo_miles, na.rm = TRUE), .groups = 'drop'),
    by = c("iso3", "anio")
  ) %>%
  mutate(
    # evitar división por cero si empleo_total_anual_pais_global es 0 o NA
    proporcion_servicios_en_pais = ifelse(is.na(empleo_total_anual_pais_global) | empleo_total_anual_pais_global == 0,
                                          0,
                                          empleo_total_gran_sector / empleo_total_anual_pais_global)
  ) %>%
  ungroup() %>%
  filter(!is.na(proporcion_servicios_en_pais)) # Eliminar filas donde la proporción no se pudo calcular


# creación del gráfico con ggplot2
grafico_servicios_comparativo <- ggplot(datos_procesados_servicios,
                                        aes(x = anio,
                                            y = proporcion_servicios_en_pais,
                                            color = iso3, # Color por país
                                            group = iso3, # Agrupar por país para líneas correctas
                                            # Definir text para tooltip personalizado en plotly
                                            text = paste("País:", iso3,
                                                         "<br>Año:", anio,
                                                         "<br>Sector:", gran_sector,
                                                         "<br>Proporción en Empleo Total del País:", scales::percent(proporcion_servicios_en_pais, accuracy = 0.1))
                                        )) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  # geom_point(size = 1.5, alpha = 0.6) + # Opcional: añadir puntos
  # Línea vertical para el evento
  geom_vline(xintercept = anio_evento, linetype = "dashed", color = "red", linewidth = 0.7) +
  # Anotación para la línea vertical
  # Ajustar y_posicion_annotation y x_posicion_annotation según sea necesario para que no se superponga mucho
  annotate("text", x = anio_evento + 0.5, y = max(datos_procesados_servicios$proporcion_servicios_en_pais, na.rm = TRUE) * 0.95, # Posición de la etiqueta, añadido na.rm = TRUE a max()
           label = etiqueta_evento, color = "red", size = 3, hjust = 0) + # hjust=0 para alinear a la izquierda del punto
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + # Más cortes en el eje x si es necesario
  labs(
    title = paste("Proporción del Empleo del Sector 'Servicios' en el Empleo Total del País"),
    subtitle = paste("Años:", anio_inicio, "-", anio_fin, ". Países:", paste(paises_seleccionados, collapse=", ")),
    x = "Año",
    y = paste("Proporción del Empleo en", sector_analizado),
    color = "País (ISO3)" # Leyenda del color
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top", # Mover leyenda arriba para más espacio
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    plot.subtitle = element_text(hjust = 0.5, size=10),
    axis.text.x = element_text(angle = 45, hjust = 1, size=9),
    axis.title = element_text(size=10),
    panel.grid.minor = element_blank() # Quitar líneas menores de la cuadrícula
  )

# convertir a gráfico interactivo con plotly
grafico_servicios_interactivo <- ggplotly(grafico_servicios_comparativo, tooltip = "text")
print(grafico_servicios_interactivo)

# guardar en carpeta "resultados"
ruta_guardado_html_servicios <- "bases/comparacion_empleo_servicios_1988-2018.html"
saveWidget(grafico_servicios_interactivo, file = ruta_guardado_html_servicios, selfcontained = TRUE, title = "Comparación Empleo Servicios")

  