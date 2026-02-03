#Crear grafico en lista y cuadriculas indicando que especie es la mas capturada por cada
#flota, calculado al hacer los calculos de indicadores, 
#para mayor comprension se organiza por Gear y se crean paneles


# ==========================================
# 1. CARGA DE PAQUETES Y RUTAS
# ==========================================
library(tidyverse)

main_path <- "C:/"
setwd(main_path)

# ==========================================
# 2. DEFINICIÓN DE FUNCIONES Y PARÁMETROS
# ==========================================

# Parámetros de diseño
filas_por_folio <- 27 #27 para 4 paneles, 35 para 3 
ecorregiones_fijas <- c("NAE", "TAE", "SAE")

# Paleta de colores oficial para especies
sp_cols <- c(
  'SBT' = 'darkolivegreen1', 'BFT' = 'olivedrab', 'ALB' = 'olivedrab3',
  'SWO' = 'deepskyblue4', 'BUM' = 'deepskyblue3', 'WHM' = 'lightskyblue3', 'SAI' = 'lightcyan',
  'BET' = 'yellow', 'YFT' = 'orange', 'SKJ' = 'red',
  'BSH' = 'orchid4', 'POR' = 'orchid', 'SMA' = 'orchid1', 'Other sharks' = 'pink',
  'Small Tuna' = 'snow3', 'Other tuna and tuna like species' = 'chocolate', 'Other species' = 'chocolate4'
)

# Función para preparar el orden interno (Prioridad 1-6)
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
      left_join(datos_gear %>% select(Gear, Fleet, Ecoregion, Top_Specie) %>% distinct(),
                by = c("Fleet", "Ecoregion")) %>%
      fill(Gear, .direction = "downup") %>%
      mutate(Fleet = factor(Fleet, levels = rev(orden_flotas)))
  })
}

# ==========================================
# 3. LECTURA Y PROCESAMIENTO
# ==========================================

archivos_csv <- list.files(path = main_path, pattern = "^(NAE|TAE|SAE).*\\.csv$", full.names = TRUE)
datos_flotas <- map_df(archivos_csv, function(archivo) {
  read_csv2(archivo, show_col_types = FALSE)
})

names(datos_flotas) <- make.names(names(datos_flotas))
gears_ordenados <- sort(unique(na.omit(datos_flotas$Gear)))

# Preparar datos respetando orden alfabético A-Z de Gears
datos_preparados <- map_df(gears_ordenados, function(g) {
  datos_g <- datos_flotas %>% filter(Gear == g)
  preparar_datos_multi_gear(datos_g)
})

# Lógica de paginación para que BB salga arriba en la pág 1
lista_flotas_az <- datos_preparados %>% select(Gear, Fleet) %>% distinct() %>% pull(Fleet)

datos_preparados <- datos_preparados %>%
  mutate(num_fila = match(Fleet, lista_flotas_az),
         Pagina = (num_fila - 1) %/% filas_por_folio + 1) %>%
  mutate(Fleet = factor(Fleet, levels = rev(lista_flotas_az)))
# ==========================================
# 4. BUCLE DE IMPRESIÓN DE FOLIOS (Leyenda Fija Corregida)
# ==========================================

output_folder <- file.path(main_path, "Graficos_Especies_Final_AZ")
dir.create(output_folder, showWarnings = FALSE)

# Definimos el set completo de especies
todas_las_especies <- names(sp_cols)

for (i in 1:max(datos_preparados$Pagina)) {
  datos_pag <- datos_preparados %>% filter(Pagina == i)
  n_flotas_pag <- length(unique(datos_pag$Fleet))
  
  letra_actual <- LETTERS[i]
  
  # Paso 1: Forzar los niveles del factor en los datos de la página
  datos_pag <- datos_pag %>%
    mutate(Top_Specie = factor(Top_Specie, levels = todas_las_especies))
  
  p <- ggplot(datos_pag, aes(x = factor(Ecoregion, levels = ecorregiones_fijas), 
                             y = Fleet, fill = Top_Specie)) +
    geom_tile(color = "grey85", linewidth = 0.2) + 
    # Paso 2: drop = FALSE AQUÍ es el lugar correcto
    scale_fill_manual(values = sp_cols, 
                      limits = todas_las_especies,
                      drop = FALSE, 
                      na.translate = FALSE, 
                      name = "Species Group") +
    scale_x_discrete(position = "top", drop = FALSE) +
    facet_grid(Gear ~ ., scales = "free_y", space = "free_y") +
    labs(title = paste("Species Distribution by Fleet - ", letra_actual), x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      strip.text.y = element_text(angle = 0, face = "bold", size = 7),
      strip.background = element_rect(fill = "grey98", color = "grey85"),
      axis.text.x.top = element_text(face = "bold", size = 9),
      axis.text.y = element_text(size = 7),
      legend.position = "bottom",
      legend.text = element_text(size = 5), 
      legend.key.size = unit(0.25, "cm"),
      panel.spacing = unit(0, "lines"),
      panel.border = element_rect(color = "grey85", fill = NA),
      panel.grid = element_blank()
    ) +
    # Paso 3: Limpiamos guide_legend de argumentos inválidos
    guides(fill = guide_legend(nrow = 2, 
                               byrow = TRUE, 
                               title.position = "top", 
                               title.hjust = 0.5))
  
  altura_final <- ifelse(n_flotas_pag < filas_por_folio, 
                         (n_flotas_pag / filas_por_folio) * 11.69 + 2, 
                         11.69)
  
  ggsave(filename = file.path(output_folder, paste0("Folio_Especies_", letra_actual, ".png")),
         plot = p, width = 8.27, height = max(5, altura_final), dpi = 300)
}

message("¡Proceso completado! Sin avisos y con leyenda completa de 2 filas.")