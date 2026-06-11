# ------------------------------------------------------------
# Bioestadística en R
# Laboratorio 06
# Graficos mejorados del lab 05 en ggplot
# Beatriz García Valenciano
# ------------------------------------------------------------

library(ggplot2)

# ------------------------------------------------------------
# TEMA DE PUBLICACION
# ------------------------------------------------------------

tema_revista <- theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.text = element_text(
      size = 10,
      color = "black"
    ),
    
    axis.line = element_line(
      linewidth = 0.6
    ),
    
    legend.position = "none",
    
    plot.title = element_blank()
  )

# ------------------------------------------------------------
# FIGURA 1
# TEMPERATURA DEL SUELO VS RESPIRACION DEL SUELO
# ------------------------------------------------------------

fig1 <- ggplot(
  datos_limpios,
  aes(
    x = soil_temp,
    y = soil_respiration
  )
) +
  geom_point(
    size = 2.8,
    alpha = 0.75,
    color = "#1B4F72"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    color = "black",
    fill = "grey80"
  ) +
  labs(
    x = "Temperatura del suelo (°C)",
    y = "Respiración del suelo"
  ) +
  tema_revista

fig1

# ------------------------------------------------------------
# FIGURA 2
# HUMEDAD DEL SUELO VS RESPIRACION DEL SUELO
# ------------------------------------------------------------

fig2 <- ggplot(
  datos_limpios,
  aes(
    x = soil_moisture,
    y = soil_respiration
  )
) +
  geom_point(
    size = 2.8,
    alpha = 0.75,
    color = "#117A65"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    color = "black",
    fill = "grey80"
  ) +
  labs(
    x = "Humedad del suelo (%)",
    y = "Respiración del suelo"
  ) +
  tema_revista

fig2

# ------------------------------------------------------------
# FIGURA 3
# NITROGENO DEL SUELO VS RESPIRACION DEL SUELO
# ------------------------------------------------------------

fig3 <- ggplot(
  datos_limpios,
  aes(
    x = soil_n,
    y = soil_respiration
  )
) +
  geom_point(
    size = 2.8,
    alpha = 0.75,
    color = "#7D6608"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    color = "black",
    fill = "grey80"
  ) +
  labs(
    x = "Nitrógeno del suelo (%)",
    y = "Respiración del suelo"
  ) +
  tema_revista

fig3

# ------------------------------------------------------------
# FIGURA 4
# BIOMASA MICROBIANA VS RESPIRACION DEL SUELO
# ------------------------------------------------------------

fig4 <- ggplot(
  datos_limpios,
  aes(
    x = microbial_biomass,
    y = soil_respiration
  )
) +
  geom_point(
    size = 2.8,
    alpha = 0.75,
    color = "#6C3483"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    color = "black",
    fill = "grey80"
  ) +
  labs(
    x = "Biomasa microbiana",
    y = "Respiración del suelo"
  ) +
  tema_revista

fig4

# ------------------------------------------------------------
# FIGURA 5
# OBSERVADO VS PREDICHO
# ------------------------------------------------------------

datos_pred <- data.frame(
  observado = datos_limpios$soil_respiration,
  predicho = pred
)

fig5 <- ggplot(
  datos_pred,
  aes(
    x = observado,
    y = predicho
  )
) +
  geom_point(
    size = 2.8,
    alpha = 0.75,
    color = "#922B21"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  labs(
    x = "Respiración observada",
    y = "Respiración predicha"
  ) +
  tema_revista

fig5

# ------------------------------------------------------------
# EXPORTAR FIGURAS
# CALIDAD DE PUBLICACION
# ------------------------------------------------------------

ggsave(
  "Figura1_Temperatura_Respiracion.tiff",
  plot = fig1,
  width = 8.5,
  height = 7,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "Figura2_Humedad_Respiracion.tiff",
  plot = fig2,
  width = 8.5,
  height = 7,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "Figura3_Nitrogeno_Respiracion.tiff",
  plot = fig3,
  width = 8.5,
  height = 7,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "Figura4_Biomasa_Respiracion.tiff",
  plot = fig4,
  width = 8.5,
  height = 7,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "Figura5_Observado_Predicho.tiff",
  plot = fig5,
  width = 8.5,
  height = 7,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

# ------------------------------------------------------------
# FIN
# ------------------------------------------------------------