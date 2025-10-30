# ==========================================
# Diagnóstico de NAs nas métricas
# ==========================================

library(dplyr)
library(readr)

dir_metricas <- "resultados/metricas/"

# Carrega os arquivos de métricas
metricas_por_base <- read_csv(file.path(dir_metricas, "metricas_por_base.csv"), show_col_types = FALSE)
metricas_resumo   <- read_csv(file.path(dir_metricas, "metricas_resumo.csv"), show_col_types = FALSE)

# Conta NAs por coluna
cat("\n--- NAs em metricas_por_base ---\n")
metricas_por_base %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  t() %>%
  as.data.frame() %>%
  rename(NAs = V1) %>%
  print()

cat("\n--- NAs em metricas_resumo ---\n")
metricas_resumo %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  t() %>%
  as.data.frame() %>%
  rename(NAs = V1) %>%
  print()

# Percentual total de valores ausentes
perc_na <- function(df) round(100 * sum(is.na(df)) / (nrow(df) * ncol(df)), 2)

cat("\nPercentual total de NAs:\n")
cat("metricas_por_base:", perc_na(metricas_por_base), "%\n")
cat("metricas_resumo:", perc_na(metricas_resumo), "%\n")
