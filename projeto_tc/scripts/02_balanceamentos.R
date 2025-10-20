# ==========================================
# Script: 02_balanceamentos.R
# Objetivo: Gerar versões balanceadas das bases pré-processadas
# Métodos: ROSE, SMOTE (smotefamily), Subamostragem Aleatória (SDA)
# Entrada: data/processed/lista_bases_raw.RData
# Saída:   data/processed/lista_bases_[metodo].RData
# ==========================================

rm(list = ls())

# Pacotes necessários
library(smotefamily)
library(ROSE)
library(dplyr)
library(caret)

# Caminhos
dir_processed <- "data/processed/"
arquivo_raw <- file.path(dir_processed, "lista_bases_raw.RData")

# Carrega as bases pré-processadas
if (!file.exists(arquivo_raw)) {
  stop("⚠️ O arquivo 'lista_bases_raw.RData' não foi encontrado.
       Execute primeiro o script 01_preprocessamento.R.")
}

load(arquivo_raw)
cat("✅ Lista de bases carregada com sucesso!\n")

# ============================================================
# Funções de balanceamento
# ============================================================

# 1️⃣ SMOTE (usando smotefamily)
balancear_smote <- function(df, k = 5) {
  if (!"Class" %in% names(df)) stop("Variável alvo 'Class' não encontrada.")
  
  # Garante que a classe seja fator
  df$Class <- as.factor(df$Class)
  
  # Converte para matriz numérica (smotefamily exige isso)
  df_num <- df %>% select(-Class)
  df_num <- as.data.frame(lapply(df_num, as.numeric))
  
  # Aplica SMOTE
  tryCatch({
    smote_result <- SMOTE(df_num, df$Class, K = k)
    dados_sinteticos <- smote_result$data
    names(dados_sinteticos)[ncol(dados_sinteticos)] <- "Class"
    dados_sinteticos$Class <- as.factor(dados_sinteticos$Class)
    return(dados_sinteticos)
  }, error = function(e) {
    warning(paste("Erro ao aplicar SMOTE:", e$message))
    return(df)
  })
}

# 2️⃣ ROSE
balancear_rose <- function(df) {
  if (!"Class" %in% names(df)) stop("Variável alvo 'Class' não encontrada.")
  tryCatch({
    df_bal <- ROSE(Class ~ ., data = df, seed = 123)$data
    return(df_bal)
  }, error = function(e) {
    warning(paste("Erro ao aplicar ROSE:", e$message))
    return(df)
  })
}

# 3️⃣ Undersampling Aleatório
balancear_undersampling <- function(df) {
  if (!"Class" %in% names(df)) stop("Variável alvo 'Class' não encontrada.")
  tryCatch({
    classes <- table(df$Class)
    n_min <- min(classes)
    df_bal <- df %>%
      group_by(Class) %>%
      sample_n(n_min) %>%
      ungroup()
    return(df_bal)
  }, error = function(e) {
    warning(paste("Erro ao aplicar undersampling:", e$message))
    return(df)
  })
}

# 4️⃣ Oversampling Aleatório
balancear_oversampling <- function(df) {
  if (!"Class" %in% names(df)) stop("Variável alvo 'Class' não encontrada.")
  tryCatch({
    classes <- table(df$Class)
    n_max <- max(classes)
    df_bal <- df %>%
      group_by(Class) %>%
      sample_n(n_max, replace = TRUE) %>%
      ungroup()
    return(df_bal)
  }, error = function(e) {
    warning(paste("Erro ao aplicar oversampling:", e$message))
    return(df)
  })
}

# ============================================================
# Aplica os balanceamentos
# ============================================================

cat("\n⚖️  Aplicando técnicas de balanceamento...\n")

bases_smote <- lapply(lista_bases_raw, balancear_smote)
bases_rose  <- lapply(lista_bases_raw, balancear_rose)
bases_under <- lapply(lista_bases_raw, balancear_undersampling)
bases_over  <- lapply(lista_bases_raw, balancear_oversampling)

# ============================================================
# Salva resultados
# ============================================================

save(bases_smote, file = file.path(dir_processed, "lista_bases_smote.RData"))
save(bases_rose,  file = file.path(dir_processed, "lista_bases_rose.RData"))
save(bases_under, file = file.path(dir_processed, "lista_bases_undersampling.RData"))
save(bases_over,  file = file.path(dir_processed, "lista_bases_oversampling.RData"))

cat("\n💾 Bases balanceadas salvas em data/processed/:")
cat("\n   - lista_bases_smote.RData")
cat("\n   - lista_bases_rose.RData")
cat("\n   - lista_bases_undersampling.RData")
cat("\n   - lista_bases_oversampling.RData\n")

cat("\n✅ Script concluído com sucesso!\n")