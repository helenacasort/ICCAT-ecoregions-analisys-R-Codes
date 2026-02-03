#este script crea un bucle para representar los tileplots temporales y de captura 
#de cada flota y las agrupa por artes para crear paneles


setwd("C:/")

library(ggplot2)
library(dplyr)
library(purrr)
library(scales)
library(patchwork)
library(forcats)

CATDIS <- read.csv2("TASK1_clean.csv")

# 1. Mapeo de grupos (Mantenemos tus siglas para que los filtros funcionen)
mapeo_grupos <- c(
  "BB" = "Hook and lines", "RR" = "Hook and lines", "TR" = "Hook and lines", "TL" = "Hook and lines",
  "TW" = "Seines and trawls", "HS" = "Seines and trawls",
  "GN" = "Gillnets", "TN" = "Gillnets",
  "TP" = "Others", "HP" = "Others", "FA" = "Others", "UN" = "Others",
  "LL" = "LL", "PS" = "PS", "HL" = "HL"
)

# 2. Carga y preparación
CATDIS_grouped <- CATDIS %>%
  mutate(Grupo_Plot = mapeo_grupos[GearGrp]) %>%
  filter(!is.na(Grupo_Plot))

# 3. FUNCIÓN BASE MEJORADA
crear_plot_base <- function(nombre_grupo) {
  datos_plot <- CATDIS_grouped %>% 
    filter(Grupo_Plot == nombre_grupo) %>% 
    group_by(Gear_Fleet_New, YearC) %>% 
    summarize(Total_Catch = sum(as.numeric(Qty_t), na.rm = TRUE), .groups = "drop")
  
  if(nrow(datos_plot) == 0) return(NULL)
  
  n_flotas <- length(unique(datos_plot$Gear_Fleet_New))
  tamano_fuente_y <- case_when(n_flotas > 50 ~ 3, n_flotas > 30 ~ 4, TRUE ~ 5.5)
  gears_incluidos <- paste(names(mapeo_grupos)[mapeo_grupos == nombre_grupo], collapse = ", ")
  
  ggplot(datos_plot, aes(x = YearC, y = fct_rev(reorder(Gear_Fleet_New, Gear_Fleet_New)), fill = Total_Catch)) +
    geom_tile(color = "white", linewidth = 0.05) +
    geom_vline(xintercept = 2014.5, linetype = "dashed", color = "red", linewidth = 0.5) +
    # ESCALA CON NÚMEROS CORTOS (K para mil, M para millón)
    scale_fill_gradient(
      low = "#e0f3f8", high = "#084594", 
      name = 'Catch (T)', 
      labels = label_number(scale_cut = cut_short_scale()),
      guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 0.5)
    ) +
    scale_x_continuous(breaks = seq(min(datos_plot$YearC), max(datos_plot$YearC), by = 15), expand = c(0,0)) +
    labs(title = paste0(nombre_grupo, ": ", gears_incluidos), x = "Year", y = NULL) +
    theme_minimal(base_size = 8) + # Base pequeña para que al unir plots no crezca el texto
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = tamano_fuente_y, color = "black"),
      axis.text.x = element_text(size = 7),
      legend.position = "bottom",
      legend.text = element_text(size = 6, angle = 30, vjust = 1, hjust = 1),
      legend.title = element_text(size = 7, face = "bold"),
      panel.grid = element_blank()
    )
}

# 4. Generar lista de plots
nombres_grupos <- unique(CATDIS_grouped$Grupo_Plot)
lista_plots <- map(nombres_grupos, crear_plot_base) %>% set_names(nombres_grupos)

i_letra <- 1 
letras_referencia <- LETTERS 

# 5. BLOQUE A: LONG LINE
if("LL" %in% nombres_grupos) {
  p_ll_single <- lista_plots[["LL"]] +
    labs(title = "LL: Longline") +
    scale_x_continuous(breaks = seq(1950, 2025, by = 5), expand = c(0,0)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    plot_annotation(tag_levels = list(letras_referencia[i_letra]))
  
  ggsave("Panel_A4_LL_Vertical_2014.png", p_ll_single, width = 21, height = 29.7, units = "cm", dpi = 600, bg = "white")
  i_letra <- i_letra + 1
}

# 6. BLOQUE B: PAREJAS
grupos_sin_LL <- nombres_grupos[nombres_grupos != "LL"]
indices <- seq(1, length(grupos_sin_LL), by = 2)

# Usamos un bucle for para controlar mejor el incremento de i_letra
for (i in indices) {
  p1 <- lista_plots[[grupos_sin_LL[i]]]
  p2 <- if(i + 1 <= length(grupos_sin_LL)) lista_plots[[grupos_sin_LL[i+1]]] else NULL
  
  if(!is.null(p2)) {
    plot_final <- (p1 | p2) + 
      plot_annotation(tag_levels = list(letras_referencia[i_letra:(i_letra+1)]))
    i_letra <- i_letra + 2
  } else {
    plot_final <- p1 + 
      plot_annotation(tag_levels = list(letras_referencia[i_letra]))
    i_letra <- i_letra + 1
  }
  
  num_archivo <- (i + 1) / 2
  ggsave(paste0("Panel_A4_Parejas_2014_", num_archivo, ".png"), plot_final, width = 29.7, height = 21, units = "cm", dpi = 300, bg = "white")
}