# ============================================================
# Script: 03_modelos.R
# Objetivo: Treinar e avaliar modelos de classificação
# Modelos: Regressão Logística, Árvore, Random Forest (ranger), SVM e Rede Neural (nnet)
# Paralelização: doParallel + foreach (com cluster reutilizável)
# ============================================================

rm(list = ls())

library(caret)
library(rpart)
library(ranger)
library(e1071)
library(LiblineaR)
library(dplyr)
library(foreach)
library(doParallel)
library(readr)
library(nnet)

# ------------------------------------------------------------
# Métricas de desempenho
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
treinar_modelo <- function(df, metodo, proporcao_treino = 0.7, nome_base = "desconhecida") {
  if (!"Class" %in% names(df)) stop("⚠️ Variável alvo 'Class' não encontrada.")
  
  set.seed(123)
  idx <- createDataPartition(df$Class, p = proporcao_treino, list = FALSE)
  treino <- df[idx, ]
  teste  <- df[-idx, ]
  
  t0 <- Sys.time()
  
  resultado <- tryCatch({
    
    # Controle de treinamento do caret
    ctrl <- trainControl(
      method = "cv",          # Validação cruzada
      number = 3,             # 3-fold
      classProbs = FALSE,     # Sem probabilidade
      verboseIter = FALSE
    )
    
    modelo <- switch(metodo,
                     "logistico" = caret::train(
                       Class ~ ., data = treino,
                       method = "glm",
                       family = binomial,
                       trControl = ctrl
                     ),
                     
                     "arvore" = caret::train(
                       Class ~ ., data = treino,
                       method = "rpart",
                       trControl = ctrl,
                       tuneLength = 3
                     ),
                     
                     "rf" = caret::train(
                       Class ~ ., data = treino,
                       method = "ranger",
                       trControl = ctrl,
                       tuneLength = 3,
                       importance = "impurity"
                     ),
                     
                     "svm" = caret::train(
                       Class ~ ., data = treino,
                       method = "svmRadial",
                       trControl = ctrl,
                       tuneLength = 3
                     ),
                     
                     "ann" = caret::train(
                       Class ~ ., data = treino,
                       method = "nnet",
                       trControl = ctrl,
                       tuneLength = 3,
                       trace = FALSE
                     ),
                     
                     stop("⚠️ Método desconhecido.")
    )
    
    # Predição
    prev <- predict(modelo, newdata = teste)
    prev <- factor(prev, levels = levels(df$Class))
    metricas <- calcular_metricas(teste$Class, prev)
    
    duracao <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    metricas$TempoSeg <- duracao
    metricas$Modelo <- metodo
    metricas$Base <- nome_base
    
    metricas
  }, error = function(e) {
    warning(paste("⚠️ Erro ao treinar modelo", metodo, ":", e$message))
    tibble(
      Acuracia = NA, Precisao = NA, Recall = NA, F1 = NA,
      TempoSeg = 0, Modelo = metodo, Base = nome_base
    )
  })
  
  return(resultado)
}



# ------------------------------------------------------------
# Função: treinar todos os modelos em uma base
# ------------------------------------------------------------
treinar_todos_modelos <- function(df, nome_base = "desconhecida") {
  dplyr::bind_rows(
    treinar_modelo(df, "logistico", nome_base = nome_base),
    treinar_modelo(df, "arvore",    nome_base = nome_base),
    treinar_modelo(df, "rf",        nome_base = nome_base),
    treinar_modelo(df, "svm",       nome_base = nome_base),
    treinar_modelo(df, "ann",       nome_base = nome_base)
  )
}

# ------------------------------------------------------------
# Funções de cluster
# ------------------------------------------------------------
iniciar_cluster <- function() {
  num_cores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  cat(paste0("Cluster inicializado com ", num_cores, " núcleos.\n"))
  return(cl)
}

finalizar_cluster <- function(cl) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    cat("Cluster finalizado.\n")
  }
}

# ------------------------------------------------------------
# Função principal: aplicar em várias bases (paralelo)
# ------------------------------------------------------------
treinar_em_lista <- function(lista_bases, cl = NULL) {
  own_cluster <- is.null(cl)
  if (own_cluster) cl <- iniciar_cluster()
  on.exit(if (own_cluster) finalizar_cluster(cl), add = TRUE)
  
  # Exporta funções e dependências
  parallel::clusterExport(cl, varlist = c(
    "treinar_todos_modelos", "treinar_modelo", "calcular_metricas"
  ), envir = environment())
  
  resultados <- foreach(
    nome_base = names(lista_bases),
    .combine = bind_rows,
    .packages = c(
      "caret", "rpart", "ranger", "e1071", "LiblineaR",
      "dplyr", "nnet"
    )
  ) %dopar% {
    df <- lista_bases[[nome_base]]
    res <- treinar_todos_modelos(df, nome_base = nome_base)
    cat(paste0("✅ Concluído: ", nome_base, "\n"))
    res
  }
  
  cat("✅ Treinamento paralelo concluído!\n")
  
  # Salva log de tempos
  log_tempo <- resultados %>%
    group_by(Base, Modelo) %>%
    summarise(TempoMedio = mean(TempoSeg, na.rm = TRUE), .groups = "drop")
  
  dir.create("resultados/metricas", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(log_tempo, file.path("resultados/metricas", "tempo_execucao_modelos.csv"))
  
  return(resultados)
}
