# ------------------------------------------------------------
# Bioestadística en R
# Laboratorio 05
# Regresión múltiple y selección de modelos
# Beatriz García Valenciano
# ------------------------------------------------------------

# ------------------------------------------------------------
# 1. PAQUETES
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 2. IMPORTAR DATOS
# ------------------------------------------------------------

datos <- read_excel(
  "Lab04/04_respiracion_suelo_bosques.xlsx"
)

# ------------------------------------------------------------
# 3. RENOMBRAR VARIABLES
# ------------------------------------------------------------

names(datos) <- c(
  "plot_id",
  "site",
  "block",
  "land_use",
  "successional_age",
  "soil_respiration",
  "soil_temp",
  "soil_moisture",
  "ph",
  "organic_matter",
  "soil_c",
  "soil_n",
  "cn_ratio",
  "bulk_density",
  "canopy_cover",
  "litter_depth",
  "litter_mass",
  "root_biomass",
  "microbial_biomass",
  "enzyme_activity",
  "decomposition",
  "basal_area",
  "tree_density",
  "species_richness",
  "shannon",
  "soil_fauna",
  "high_respiration"
)

# ------------------------------------------------------------
# 4. LIMPIEZA GENERAL
# ------------------------------------------------------------

datos <- datos %>%
  mutate(
    soil_respiration = as.numeric(soil_respiration),
    soil_moisture = as.numeric(soil_moisture),
    ph = as.numeric(ph),
    
    plot_id = factor(plot_id),
    site = factor(site),
    block = factor(block)
  )

# ------------------------------------------------------------
# 5. CORREGIR VALORES ERRÓNEOS
# ------------------------------------------------------------

datos$soil_moisture[
  datos$soil_moisture == -999
] <- NA

# ------------------------------------------------------------
# 6. ESTANDARIZAR COBERTURAS
# ------------------------------------------------------------

datos$land_use <- trimws(
  tolower(
    as.character(datos$land_use)
  )
)

datos$land_use[
  grepl("degrad", datos$land_use)
] <- "Degradado"

datos$land_use[
  grepl("second", datos$land_use) |
    grepl("secund", datos$land_use)
] <- "Bosque secundario"

datos$land_use[
  grepl("primary", datos$land_use) |
    grepl("primry", datos$land_use)
] <- "Bosque primario"

datos$land_use[
  is.na(datos$land_use) &
    datos$successional_age < 10
] <- "Degradado"

datos$land_use[
  is.na(datos$land_use) &
    datos$successional_age >= 10 &
    datos$successional_age < 60
] <- "Bosque secundario"

datos$land_use[
  is.na(datos$land_use) &
    datos$successional_age >= 60
] <- "Bosque primario"

datos$land_use <- factor(
  datos$land_use,
  levels = c(
    "Degradado",
    "Bosque secundario",
    "Bosque primario"
  )
)

# ------------------------------------------------------------
# 7. BASE LIMPIA
# ------------------------------------------------------------

datos_limpios <- datos %>%
  filter(
    !is.na(soil_respiration),
    !is.na(soil_moisture),
    !is.na(ph),
    !is.na(land_use)
  )

# ------------------------------------------------------------
# 8. REVISIÓN GENERAL
# ------------------------------------------------------------

str(datos_limpios)

summary(datos_limpios)

colSums(is.na(datos_limpios))

# ------------------------------------------------------------
# 9. EXPLORACIÓN GRÁFICA
# ------------------------------------------------------------

ggplot(
  datos_limpios,
  aes(
    soil_temp,
    soil_respiration
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  theme_minimal() +
  labs(
    x = "Temperatura del suelo",
    y = "Respiración del suelo"
  )

ggplot(
  datos_limpios,
  aes(
    soil_moisture,
    soil_respiration
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  theme_minimal() +
  labs(
    x = "Humedad del suelo",
    y = "Respiración del suelo"
  )

ggplot(
  datos_limpios,
  aes(
    soil_n,
    soil_respiration
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  theme_minimal() +
  labs(
    x = "Nitrógeno del suelo",
    y = "Respiración del suelo"
  )

ggplot(
  datos_limpios,
  aes(
    microbial_biomass,
    soil_respiration
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  theme_minimal() +
  labs(
    x = "Biomasa microbiana",
    y = "Respiración del suelo"
  )

# ------------------------------------------------------------
# 10. MATRIZ DE CORRELACIÓN
# ------------------------------------------------------------

numericas <- datos_limpios %>%
  select(
    soil_respiration,
    soil_temp,
    soil_moisture,
    soil_c,
    soil_n,
    canopy_cover,
    litter_mass,
    root_biomass,
    microbial_biomass,
    enzyme_activity,
    decomposition
  )

correlacion <- cor(
  numericas,
  use = "complete.obs"
)

round(correlacion, 2)

# ------------------------------------------------------------
# 11. COLINEALIDAD
# ------------------------------------------------------------

modelo_vif <- lm(
  soil_respiration ~
    soil_temp +
    soil_moisture +
    soil_c +
    soil_n +
    canopy_cover +
    litter_mass +
    root_biomass +
    microbial_biomass +
    enzyme_activity +
    decomposition,
  data = datos_limpios
)

library(car)

vif(modelo_vif)

# ------------------------------------------------------------
# 12. MODELOS CANDIDATOS
# ------------------------------------------------------------

modelo1 <- lm(
  soil_respiration ~
    soil_temp +
    soil_moisture,
  data = datos_limpios
)

modelo2 <- lm(
  soil_respiration ~
    soil_temp +
    soil_moisture +
    soil_c +
    soil_n,
  data = datos_limpios
)

modelo3 <- lm(
  soil_respiration ~
    soil_temp +
    soil_moisture +
    soil_c +
    soil_n +
    canopy_cover +
    litter_mass,
  data = datos_limpios
)

modelo4 <- lm(
  soil_respiration ~
    soil_temp +
    soil_moisture +
    soil_c +
    soil_n +
    canopy_cover +
    litter_mass +
    root_biomass +
    microbial_biomass +
    enzyme_activity +
    decomposition,
  data = datos_limpios
)

# ------------------------------------------------------------
# 13. RESUMEN DE MODELOS
# ------------------------------------------------------------

summary(modelo1)

summary(modelo2)

summary(modelo3)

summary(modelo4)

# ------------------------------------------------------------
# 14. COMPARACIÓN DE MODELOS
# ------------------------------------------------------------

comparacion <- data.frame(
  Modelo = c(
    "Modelo 1",
    "Modelo 2",
    "Modelo 3",
    "Modelo 4"
  ),
  
  Parametros = c(
    length(coef(modelo1)),
    length(coef(modelo2)),
    length(coef(modelo3)),
    length(coef(modelo4))
  ),
  
  AIC = c(
    AIC(modelo1),
    AIC(modelo2),
    AIC(modelo3),
    AIC(modelo4)
  ),
  
  BIC = c(
    BIC(modelo1),
    BIC(modelo2),
    BIC(modelo3),
    BIC(modelo4)
  ),
  
  R2_Ajustado = c(
    summary(modelo1)$adj.r.squared,
    summary(modelo2)$adj.r.squared,
    summary(modelo3)$adj.r.squared,
    summary(modelo4)$adj.r.squared
  )
)

comparacion

# ------------------------------------------------------------
# 15. SELECCIÓN STEPWISE
# ------------------------------------------------------------

modelo_final <- step(
  modelo4,
  direction = "both",
  trace = FALSE
)

summary(modelo_final)

# ------------------------------------------------------------
# 16. DIAGNÓSTICOS DEL MODELO FINAL
# ------------------------------------------------------------

par(mfrow = c(2,2))

plot(modelo_final)

par(mfrow = c(1,1))

# ------------------------------------------------------------
# 17. VALORES OBSERVADOS VS PREDICHOS
# ------------------------------------------------------------

pred <- predict(
  modelo_final
)

ggplot(
  data.frame(
    observado = datos_limpios$soil_respiration,
    predicho = pred
  ),
  aes(
    observado,
    predicho
  )
) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2
  ) +
  theme_minimal() +
  labs(
    x = "Observado",
    y = "Predicho"
  )


# ------------------------------------------------------------
# FIN DEL SCRIPT
# ------------------------------------------------------------