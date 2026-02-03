#Crear grafico en lista y cuadriculas indicando que tipo de flota es cada una
#regional o across-regions, para mayor comprension se organiza por Gear y se crean paneles

# 1. Cargar paquetes necesarios
library(tidyverse)

# 2. Configurar rutas
main_path <- "C:/"
setwd(main_path)

# 3. Lectura de datos (Tu estructura limpia)
archivos_csv <- list.files(path = main_path, pattern = "^(NAE|TAE|SAE).*\\.csv$", full.names = TRUE)

datos_flotas <- map_df(archivos_csv, function(archivo) {
  temp_df <- read_csv2(archivo, show_col_types = FALSE)
  tipo <- ifelse(str_detect(basename(archivo), "across"), "Across-regions", "Regional")
  temp_df %>% mutate(Type = tipo)
})

names(datos_flotas) <- make.names(names(datos_flotas))
type_colors <- c("Regional" = "lightblue4", "Across-regions" = "black")
ecorregiones_fijas <- c("NAE", "TAE", "SAE")

# 4. Función de preparación con ordenación por Gear
preparar_datos_multi_gear <- function(df_subset) {
  df_list <- df_subset %>% split(.$Gear)
  
  map_df(df_list, function(datos_gear) {
    orden_flotas <- datos_gear %>%
      group_by(Fleet) %>%
      summarise(
        n = n_distinct(Ecoregion),
        nae = any(Ecoregion == "NAE"),
        tae = any(Ecoregion == "TAE"),
        sae = any(Ecoregion == "SAE"),
        .groups = "drop"
      ) %>%
      mutate(
        grupo_prioridad = case_when(
          nae & n == 1           ~ 1,
          nae & n > 1            ~ 2,
          tae & n > 1 & !nae     ~ 3,
          tae & n == 1           ~ 4,
          sae & n > 1 & !nae & !tae ~ 5,
          sae & n == 1           ~ 6,
          TRUE                   ~ 7
        )
      ) %>%
      arrange(grupo_prioridad, Fleet) %>%
      pull(Fleet) %>%
      unique()
    
    cuadricula_gear <- expand_grid(Fleet = orden_flotas, Ecoregion = ecorregiones_fijas)
    
    cuadricula_gear %>%
      left_join(datos_gear %>% select(Gear, Fleet, Ecoregion, Type) %>% distinct(),
                by = c("Fleet", "Ecoregion")) %>%
      fill(Gear, .direction = "downup") %>%
      mutate(Fleet = factor(Fleet, levels = rev(orden_flotas)))
  })
}

# ==========================================
# 3. CONFIGURACIÓN Y ORDEN
# ==========================================

# Parámetros de diseño
filas_por_folio <- 27 #27 para 4 paneles, 35 para 3
type_colors <- c("Regional" = "lightblue4", "Across-regions" = "black")
ecorregiones_fijas <- c("NAE", "TAE", "SAE")

# 1. Obtener lista de Gears en orden alfabético A-Z
gears_ordenados <- sort(unique(na.omit(datos_flotas$Gear)))

# 2. Preparar los datos respetando ese orden Gear por Gear
datos_preparados <- map_df(gears_ordenados, function(g) {
  datos_g <- datos_flotas %>% filter(Gear == g)
  # Usamos tu función original para el orden interno 1-6 de cada Gear
  preparar_datos_multi_gear(datos_g) 
})

# 3. TRUCO PARA EL ORDEN VISUAL (BB arriba y A-Z hacia abajo)
# Extraemos la lista de flotas tal cual han quedado tras el map_df (ya vienen A-Z)
lista_flotas_az <- datos_preparados %>%
  select(Gear, Fleet) %>%
  distinct() %>%
  pull(Fleet)

# Calculamos página y fila, y REVERTIMOS los niveles para el eje Y de ggplot
datos_preparados <- datos_preparados %>%
  mutate(num_fila = match(Fleet, lista_flotas_az),
         Pagina = (num_fila - 1) %/% filas_por_folio + 1) %>%
  mutate(Fleet = factor(Fleet, levels = rev(lista_flotas_az)))

# ==========================================
# 4. BUCLE DE IMPRESIÓN FINAL
# ==========================================
output_folder <- file.path(main_path, "Graficos_Finales_AZ")
dir.create(output_folder, showWarnings = FALSE)

num_paginas <- max(datos_preparados$Pagina)

# ... [Resto del código anterior se mantiene igual] ...

for (i in 1:num_paginas) {
  datos_pag <- datos_preparados %>% filter(Pagina == i)
  n_flotas_pag <- length(unique(datos_pag$Fleet))
  
  # Convertimos el número de página a letra (1=A, 2=B, etc.)
  letra_actual <- LETTERS[i]
  
  p <- ggplot(datos_pag, aes(x = factor(Ecoregion, levels = ecorregiones_fijas), 
                             y = Fleet, fill = Type)) +
    geom_tile(color = "grey85", linewidth = 0.2) + 
    scale_fill_manual(values = type_colors, na.translate = FALSE) +
    scale_x_discrete(position = "top", drop = FALSE) +
    facet_grid(Gear ~ ., scales = "free_y", space = "free_y") +
    # AQUÍ SE CAMBIA EL TÍTULO DEL PANEL
    labs(title = paste("Fleet Composition Comparison - ", letra_actual), 
         x = NULL, y = NULL, fill = "Type") +
    theme_minimal() +
    theme(
      strip.text.y = element_text(angle = 0, face = "bold", size = 7),
      strip.background = element_rect(fill = "grey98", color = "grey85"),
      axis.text.x.top = element_text(face = "bold", size = 9),
      axis.text.y = element_text(size = 7),
      legend.position = "bottom",
      panel.spacing = unit(0, "lines"),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey85", fill = NA)
    )
  
  altura_final <- ifelse(n_flotas_pag < filas_por_folio, 
                         (n_flotas_pag / filas_por_folio) * 11.69 + 1.5, 
                         11.69)
  
  # También ajustamos el nombre del archivo para que coincida
  ggsave(filename = file.path(output_folder, paste0("Folio_AZ_", letra_actual, ".png")),
         plot = p, width = 8.27, height = max(4, altura_final), dpi = 300)
}

message("¡Todo listo! Los archivos están en la carpeta 'Graficos_Finales_AZ'")