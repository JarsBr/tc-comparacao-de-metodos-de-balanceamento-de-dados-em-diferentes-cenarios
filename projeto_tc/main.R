# ==========================================
# Script principal: main.R
# Objetivo: Executar todo o pipeline do projeto TC
# Etapas:
#   1. Pré-processamento das bases
#   2. Balanceamentos (SMOTE, ROSE, SDA)
#   3. Treinamento e avaliação de modelos
#   4. Análises e geração de gráficos
# ==========================================

# Limpa ambiente e define opções gerais
rm(list = ls())
options(scipen = 999)
set.seed(123)

# Pacotes base
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# Caminhos e organização
# ------------------------------------------------------------
dir_scripts <- "scripts/"
dir_processed <- "data/processed/"
dir_resultados <- "resultados/"

if (!dir.exists(dir_processed)) dir.create(dir_processed, recursive = TRUE)
if (!dir.exists(dir_resultados)) dir.create(dir_resultados, recursive = TRUE)

cat("🚀 Iniciando pipeline completo do projeto TC...\n\n")

# ------------------------------------------------------------
# 1️⃣ Pré-processamento das bases
# ------------------------------------------------------------
cat("🧹 [1/4] Pré-processamento das bases...\n")
sys.source(file.path(dir_scripts, "01_preprocessamento.R"), envir = globalenv())
cat("✅ Pré-processamento concluído!\n\n")

# ------------------------------------------------------------
# 2️⃣ Balanceamentos
# ------------------------------------------------------------
cat("⚖️ [2/4] Aplicando técnicas de balanceamento...\n")
dir_scripts <- "scripts/"
dir_processed <- "data/processed/"
dir_resultados <- "resultados/"
sys.source(file.path(dir_scripts, "02_balanceamentos.R"), envir = globalenv())
cat("✅ Balanceamentos concluídos!\n\n")

# ------------------------------------------------------------
# 3️⃣ Modelos
# ------------------------------------------------------------
cat("🤖 [3/4] Treinando e avaliando modelos...\n")
dir_scripts <- "scripts/"
sys.source(file.path(dir_scripts, "03_modelos.R"), envir = globalenv())
dir_processed <- "data/processed/"
dir_resultados <- "resultados/"

load(file.path(dir_processed, "lista_bases_raw.RData"))
load(file.path(dir_processed, "lista_bases_smote.RData"))
load(file.path(dir_processed, "lista_bases_rose.RData"))
load(file.path(dir_processed, "lista_bases_sda.RData"))

resultados_raw   <- treinar_em_lista(lista_bases_raw)
resultados_smote <- treinar_em_lista(bases_smote)
resultados_rose  <- treinar_em_lista(bases_rose)
resultados_sda   <- treinar_em_lista(bases_sda)

save(resultados_raw,   file = file.path(dir_processed, "resultados_raw.RData"))
save(resultados_smote, file = file.path(dir_processed, "resultados_smote.RData"))
save(resultados_rose,  file = file.path(dir_processed, "resultados_rose.RData"))
save(resultados_sda,   file = file.path(dir_processed, "resultados_sda.RData"))

cat("✅ Modelos treinados e resultados salvos!\n\n")

# ------------------------------------------------------------
# 4️⃣ Análises e geração de gráficos
# ------------------------------------------------------------
cat("📊 [4/4] Gerando análises e gráficos...\n")
dir_scripts <- "scripts/"
sys.source(file.path(dir_scripts, "04_analises.R"), envir = globalenv())
cat("✅ Análises concluídas!\n\n")

cat("🎉 Pipeline completo executado com sucesso!\n")

# ------------------------------------------------------------
# Finalização
# ------------------------------------------------------------
cat("🎉 Pipeline completo executado com sucesso!\n")
cat("📁 Resultados disponíveis em:\n")
cat("   - data/processed/ (dados intermediários)\n")
cat("   - resultados/metricas/ e resultados/graficos/\n\n")
