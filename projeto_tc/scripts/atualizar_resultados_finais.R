# ============================================================
# Script: atualizar_resultados_finais.R
# Objetivo: Substituir resultados apenas das bases corrigidas
# Bases:
#   - 02-Breast Cancer Wisconsin (Diagnostic)
#   - 07-Oil Spill Imbalanced Classification
# ============================================================

rm(list = ls())

library(dplyr)

dir_processed <- "data/processed"

bases_problema <- c(
  "02-Breast Cancer Wisconsin (Diagnostic)",
  "07-Oil Spill Imbalanced Classification"
)

# ------------------------------------------------------------
# Função auxiliar: atualiza um arquivo de resultados
# ------------------------------------------------------------
atualizar_resultado <- function(arquivo_antigo, arquivo_corrigido, arquivo_saida) {
  
  antigo <- readRDS(file.path(dir_processed, arquivo_antigo))
  corrigido <- readRDS(file.path(dir_processed, arquivo_corrigido))
  
  atualizado <- antigo %>%
    filter(!Base %in% bases_problema) %>%
    bind_rows(corrigido)
  
  saveRDS(atualizado, file.path(dir_processed, arquivo_saida))
  
  cat("✔ Atualizado:", arquivo_saida, "\n")
}

# ------------------------------------------------------------
# Atualiza cada técnica
# ------------------------------------------------------------
atualizar_resultado(
  "resultados_raw.rds",
  "resultados_raw_corrigidos.rds",
  "resultados_raw_final.rds"
)

atualizar_resultado(
  "resultados_smote.rds",
  "resultados_smote_corrigidos.rds",
  "resultados_smote_final.rds"
)

atualizar_resultado(
  "resultados_rose.rds",
  "resultados_rose_corrigidos.rds",
  "resultados_rose_final.rds"
)

atualizar_resultado(
  "resultados_undersampling.rds",
  "resultados_undersampling_corrigidos.rds",
  "resultados_undersampling_final.rds"
)

atualizar_resultado(
  "resultados_oversampling.rds",
  "resultados_oversampling_corrigidos.rds",
  "resultados_oversampling_final.rds"
)

cat("\n✅ Resultados finais atualizados com sucesso!\n")
