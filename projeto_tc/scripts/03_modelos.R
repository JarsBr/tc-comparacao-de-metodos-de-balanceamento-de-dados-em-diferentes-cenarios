# ==========================================
# Script: 03_modelos.R
# Objetivo: Treinar e avaliar modelos de classificação
# Modelos: Regressão Logística, Árvore de Decisão, Random Forest
# Entrada: lista de bases (pré-processadas ou balanceadas)
# Saída: lista de métricas por base
# ==========================================

rm(list = ls())

library(caret)
library(rpart)
library(randomForest)
library(dplyr)

# ------------------------------------------------------------
# Função auxiliar: calcular métricas
# ------------------------------------------------------------
calcular_metricas <- function(real, previsto) {
  cm <- confusionMatrix(data = previsto, reference = real, positive = levels(real)[2])
  
  tibble(
    Acuracia = as.numeric(cm$overall["Accuracy"]),
    Precisao = as.numeric(cm$byClass["Precision"]),
    Recall   = as.numeric(cm$byClass["Recall"]),
    F1       = as.numeric(cm$byClass["F1"])
  )
}

# ------------------------------------------------------------
# Função: treinar e avaliar um modelo genérico
# ------------------------------------------------------------
treinar_modelo <- function(df, metodo, proporcao_treino = 0.7) {
  if (!"Class" %in% names(df)) stop("Variável alvo 'Class' não encontrada.")
  
  set.seed(123)
  idx <- createDataPartition(df$Class, p = proporcao_treino, list = FALSE)
  treino <- df[idx, ]
  teste  <- df[-idx, ]
  
  resultado <- tryCatch({
    # Treinamento
    modelo <- switch(metodo,
                     "logistico" = glm(Class ~ ., data = treino, family = binomial),
                     "arvore"    = rpart(Class ~ ., data = treino, method = "class"),
                     "rf"        = randomForest(Class ~ ., data = treino),
                     stop("Método desconhecido.")
    )
    
    # Predição
    prev <- predict(modelo, newdata = teste, type = ifelse(metodo == "logistico", "response", "class"))
    
    # Ajusta previsão para fator
    if (metodo == "logistico") {
      prev <- ifelse(prev > 0.5, levels(df$Class)[2], levels(df$Class)[1])
    }
    prev <- factor(prev, levels = levels(df$Class))
    
    # Calcula métricas
    metricas <- calcular_metricas(teste$Class, prev)
    metricas$Modelo <- metodo
    
    metricas
  }, error = function(e) {
    warning(paste("Erro ao treinar modelo", metodo, ":", e$message))
    tibble(
      Acuracia = NA, Precisao = NA, Recall = NA, F1 = NA, Modelo = metodo
    )
  })
  
  return(resultado)
}

# ------------------------------------------------------------
# Função: treinar todos os modelos em uma base
# ------------------------------------------------------------
treinar_todos_modelos <- function(df) {
  cat("⚙️ Treinando modelos...\n")
  bind_rows(
    treinar_modelo(df, "logistico"),
    treinar_modelo(df, "arvore"),
    treinar_modelo(df, "rf")
  )
}

# ------------------------------------------------------------
# Função: aplicar em várias bases
# ------------------------------------------------------------
treinar_em_lista <- function(lista_bases) {
  resultados <- lapply(names(lista_bases), function(nome_base) {
    cat("\n📦 Base:", nome_base, "\n")
    res <- treinar_todos_modelos(lista_bases[[nome_base]])
    res$Base <- nome_base
    return(res)
  })
  bind_rows(resultados)
}

# ------------------------------------------------------------
# Exemplo de uso
# ------------------------------------------------------------
# load("data/processed/lista_bases_raw.RData")
# resultados_raw <- treinar_em_lista(lista_bases_raw)
# save(resultados_raw, file = "data/processed/resultados_raw.RData")
