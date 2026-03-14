# ============================================================
# Script: rodar_bases_corrigidas.R
# Objetivo: Reexecutar modelos apenas para bases com ID removido
# Bases:
#   - 02-Breast Cancer Wisconsin (Diagnostic)
#   - 07-Oil Spill Imbalanced Classification
# ============================================================

rm(list = ls())

library(caret)
library(C50)
library(ranger)
library(e1071)
library(nnet)
library(dplyr)

# ------------------------------------------------------------
# Diretórios (iguais ao main)
# ------------------------------------------------------------
dir_scripts   <- "scripts"
dir_processed <- "data/processed"

# Ambiente isolado (igual ao main)
env_execucao <- new.env()

# ------------------------------------------------------------
# Carrega script de modelos no ambiente
# ------------------------------------------------------------
sys.source(
  file.path(dir_scripts, "03_modelos.R"),
  envir = env_execucao
)

# ------------------------------------------------------------
# Carrega bases (já corrigidas de ID)
# ------------------------------------------------------------
load(file.path(dir_processed, "lista_bases_raw.RData"),  envir = env_execucao)
load(file.path(dir_processed, "lista_bases_smote.RData"), envir = env_execucao)
load(file.path(dir_processed, "lista_bases_rose.RData"),  envir = env_execucao)
load(file.path(dir_processed, "lista_bases_undersampling.RData"), envir = env_execucao)
load(file.path(dir_processed, "lista_bases_oversampling.RData"),  envir = env_execucao)

bases_problema <- c(
  "02-Breast Cancer Wisconsin (Diagnostic)",
  "07-Oil Spill Imbalanced Classification"
)

# ------------------------------------------------------------
# Filtra SOMENTE as bases problemáticas
# ------------------------------------------------------------
with(env_execucao, {
  
  lista_bases_raw   <- lista_bases_raw[bases_problema]
  bases_smote <- bases_smote[bases_problema]
  bases_rose  <- bases_rose[bases_problema]
  bases_under <- bases_under[bases_problema]
  bases_over  <- bases_over[bases_problema]
  
  # ----------------------------------------------------------
  # Inicia cluster
  # ----------------------------------------------------------
  cl <- iniciar_cluster()
  
  cat("\n [1/5] Treinando modelos: RAW (bases corrigidas)\n")
  resultados_raw_corr <- treinar_em_lista(lista_bases_raw, cl)
  saveRDS(resultados_raw_corr,
          file.path(dir_processed, "resultados_raw_corrigidos.rds"))
  
  cat("\n [2/5] Treinando modelos: SMOTE (bases corrigidas)\n")
  resultados_smote_corr <- treinar_em_lista(bases_smote, cl)
  saveRDS(resultados_smote_corr,
          file.path(dir_processed, "resultados_smote_corrigidos.rds"))
  
  cat("\n [3/5] Treinando modelos: ROSE (bases corrigidas)\n")
  resultados_rose_corr <- treinar_em_lista(bases_rose, cl)
  saveRDS(resultados_rose_corr,
          file.path(dir_processed, "resultados_rose_corrigidos.rds"))
  
  cat("\n [4/5] Treinando modelos: UNDERSAMPLING (bases corrigidas)\n")
  resultados_under_corr <- treinar_em_lista(bases_under, cl)
  saveRDS(resultados_under_corr,
          file.path(dir_processed, "resultados_undersampling_corrigidos.rds"))
  
  cat("\n [5/5] Treinando modelos: OVERSAMPLING (bases corrigidas)\n")
  resultados_over_corr <- treinar_em_lista(bases_over, cl)
  saveRDS(resultados_over_corr,
          file.path(dir_processed, "resultados_oversampling_corrigidos.rds"))
  
  finalizar_cluster(cl)
})

cat("\n✅ Reexecução das bases corrigidas concluída com sucesso!\n")
