# ==========================================
# Script principal: main.R
# Objetivo: Executar todo o pipeline do projeto
# Etapas:
#   1. Pré-processamento
#   2. Balanceamentos (SMOTE, ROSE, Under, Over)
#   3. Treinamento e avaliação de modelos (paralelo com cluster único)
#   4. Geração de análises e gráficos
# ==========================================

rm(list = ls())
options(scipen = 999)
set.seed(123)

library(dplyr)
library(ggplot2)
library(progressr)

# ------------------------------------------------------------
# Caminhos
# ------------------------------------------------------------
dir_scripts    <- "scripts/"
dir_processed  <- "data/processed/"
dir_resultados <- "resultados/"

if (!dir.exists(dir_processed)) dir.create(dir_processed, recursive = TRUE)
if (!dir.exists(dir_resultados)) dir.create(dir_resultados, recursive = TRUE)

# ------------------------------------------------------------
# Função auxiliar para medir tempo
# ------------------------------------------------------------
medir_tempo <- function(etapa, expr) {
  cat(paste0("\nIniciando etapa: ", etapa, "...\n"))
  inicio <- Sys.time()
  eval(expr)
  fim <- Sys.time()
  duracao <- round(difftime(fim, inicio, units = "secs"), 2)
  cat(paste0("✅ Etapa '", etapa, "' concluída em ", duracao, " segundos.\n"))
  return(duracao)
}

# ------------------------------------------------------------
# Ambiente de execução isolado
# ------------------------------------------------------------
env_execucao <- new.env(parent = globalenv())

# ------------------------------------------------------------
# Registro de tempos
# ------------------------------------------------------------
tempos_execucao <- list()

cat(" Iniciando pipeline completo do projeto TC...\n")

# ------------------------------------------------------------
# 1- Pré-processamento
# ------------------------------------------------------------
tempos_execucao$preprocessamento <- medir_tempo(
  "Pré-processamento",
  quote(sys.source(file.path(dir_scripts, "01_preprocessamento.R"), envir = env_execucao))
)

# ------------------------------------------------------------
# 2- Balanceamentos
# ------------------------------------------------------------
tempos_execucao$balanceamentos <- medir_tempo(
  "Balanceamentos (SMOTE, ROSE, Under, Over)",
  quote(sys.source(file.path(dir_scripts, "02_balanceamentos.R"), envir = env_execucao))
)

# ------------------------------------------------------------
# 3- Modelos
# ------------------------------------------------------------
tempos_execucao$modelos <- medir_tempo(
  "Treinamento e Avaliação de Modelos",
  quote({
    # Carrega script de modelos (com paralelismo otimizado)
    sys.source(file.path(dir_scripts, "03_modelos.R"), envir = env_execucao)
    
    # Carrega bases processadas e balanceadas
    load(file.path(dir_processed, "lista_bases_raw.RData"),  envir = env_execucao)
    load(file.path(dir_processed, "lista_bases_smote.RData"), envir = env_execucao)
    load(file.path(dir_processed, "lista_bases_rose.RData"),  envir = env_execucao)
    load(file.path(dir_processed, "lista_bases_undersampling.RData"), envir = env_execucao)
    load(file.path(dir_processed, "lista_bases_oversampling.RData"),  envir = env_execucao)
    
    # Inicia cluster (usado em todas as execuções)
    with(env_execucao, {
      cl <- iniciar_cluster()
      
      cat("\n [1/5] Treinando modelos: RAW\n")
      resultados_raw <- treinar_em_lista(lista_bases_raw, cl)
      save(resultados_raw, file = file.path(dir_processed, "resultados_raw.RData"))
      
      cat("\n [2/5] Treinando modelos: SMOTE\n")
      resultados_smote <- treinar_em_lista(bases_smote, cl)
      save(resultados_smote, file = file.path(dir_processed, "resultados_smote.RData"))
      
      cat("\n [3/5] Treinando modelos: ROSE\n")
      resultados_rose <- treinar_em_lista(bases_rose, cl)
      save(resultados_rose, file = file.path(dir_processed, "resultados_rose.RData"))
      
      cat("\n [4/5] Treinando modelos: UNDERSAMPLING\n")
      resultados_undersampling <- treinar_em_lista(bases_under, cl)
      save(resultados_undersampling, file = file.path(dir_processed, "resultados_undersampling.RData"))
      
      cat("\n [5/5] Treinando modelos: OVERSAMPLING\n")
      resultados_oversampling <- treinar_em_lista(bases_over, cl)
      save(resultados_oversampling, file = file.path(dir_processed, "resultados_oversampling.RData"))
      
      
      # Finaliza cluster
      finalizar_cluster(cl)
    })
  })
)

# ------------------------------------------------------------
# 4- Análises e gráficos
# ------------------------------------------------------------
tempos_execucao$analises <- medir_tempo(
  "Análises e Geração de Gráficos",
  quote(sys.source(file.path(dir_scripts, "04_analises.R"), envir = env_execucao))
)

# ------------------------------------------------------------
# Resumo de tempos
# ------------------------------------------------------------
cat("\nTempo total por etapa:\n")
tempos_tabela <- data.frame(
  Etapa = names(tempos_execucao),
  Duracao_segundos = unlist(tempos_execucao)
)
print(tempos_tabela, row.names = FALSE)

tempo_total <- sum(unlist(tempos_execucao))
cat(paste0("\nTempo total de execução: ", round(tempo_total, 2),
           " segundos (≈ ", round(tempo_total / 60, 2), " minutos)\n"))

# ------------------------------------------------------------
# Finalização
# ------------------------------------------------------------
cat("\n✅ Pipeline completo executado com sucesso!\n")
cat("   Resultados disponíveis em:\n")
cat("   - data/processed/ (dados intermediários)\n")
cat("   - resultados/metricas/ e resultados/graficos/\n")
