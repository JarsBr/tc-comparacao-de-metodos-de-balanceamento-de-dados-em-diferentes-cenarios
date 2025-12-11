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
library(C50)
library(kernlab)

# ------------------------------------------------------------
# Métricas de desempenho
# ------------------------------------------------------------
calcular_metricas <- function(real, previsto) {
  cm <- caret::confusionMatrix(
    data = previsto,
    reference = real,
    positive = levels(real)[2]
  )
  
  acc  <- cm$overall["Accuracy"]
  prec <- cm$byClass["Precision"]
  rec  <- cm$byClass["Recall"]
  f1   <- cm$byClass["F1"]
  
  if (is.na(rec) || rec == 0) {
    prec <- 0
    f1   <- 0
  }
  
  if (is.na(prec)) prec <- 0
  if (is.na(f1))   f1   <- 0
  
  tibble(
    Acuracia = as.numeric(acc),
    Precisao = as.numeric(prec),
    Recall   = as.numeric(rec),
    F1       = as.numeric(f1)
  )
}

# ------------------------------------------------------------
# Função auxiliar para debugging do SVM
# ------------------------------------------------------------
checar_predictors <- function(df) {
  probs <- list()
  
  tipos <- sapply(df, class)
  probs$tipos <- tipos
  
  nas <- colSums(is.na(df))
  if (any(nas > 0)) probs$nas <- nas[nas > 0]
  
  nzv <- caret::nearZeroVar(df, saveMetrics = TRUE)
  if (any(nzv$nzv)) probs$nzv <- rownames(nzv)[nzv$nzv]
  
  consts <- names(which(sapply(df, function(x) length(unique(x[!is.na(x)])) < 2)))
  if (length(consts) > 0) probs$constantes <- consts
  
  return(probs)
}

# ------------------------------------------------------------
# Função principal de treino por modelo
# ------------------------------------------------------------
treinar_modelo <- function(df, metodo, proporcao_treino = 0.7, nome_base = "desconhecida") {
  
  if (!"Class" %in% names(df)) stop("⚠️ Variável alvo 'Class' não encontrada.")
  
  set.seed(123)
  idx <- caret::createDataPartition(df$Class, p = proporcao_treino, list = FALSE)
  treino <- df[idx, ]
  teste  <- df[-idx, ]
  
  t0 <- Sys.time()
  
  resultado <- tryCatch({
    
    ctrl <- caret::trainControl(method = "cv", number = 3, classProbs = FALSE)
    
    modelo <- switch(
      metodo,
      
      # ---------------- LOGÍSTICO ----------------
      "logistico" = caret::train(
        Class ~ ., data = treino,
        method = "glm",
        family = binomial,
        trControl = ctrl
      ),
      
      # ---------------- ÁRVORE (C5.0) ----------------
      "arvore" = caret::train(
        Class ~ ., data = treino,
        method = "C5.0",
        trControl = ctrl,
        tuneGrid = expand.grid(
          trials = c(1, 10, 20, 30),
          model = "tree",
          winnow = TRUE
        )
      ),
      
      # ---------------- RANDOM FOREST ----------------
      "rf" = caret::train(
        Class ~ ., data = treino,
        method = "ranger",
        trControl = ctrl,
        tuneLength = 3,
        importance = "impurity"
      ),
      
      # ---------------- SVM RADIAL ----------------
      "svm" = {
        
        #========= Pré-processamento exclusivo do SVM =========#
        df_train <- treino
        feats <- setdiff(names(df_train), "Class")
        
        # Fatores -> numeric
        for (v in feats) if (is.factor(df_train[[v]])) df_train[[v]] <- as.numeric(df_train[[v]])
        
        pre_proc <- caret::preProcess(df_train[feats], method = c("center", "scale", "medianImpute"))
        df_train_pp <- predict(pre_proc, df_train[feats])
        df_train_pp$Class <- df_train$Class
        
        # Preparar teste
        df_test_pp <- teste
        for (v in setdiff(names(df_test_pp), "Class"))
          if (is.factor(df_test_pp[[v]])) df_test_pp[[v]] <- as.numeric(df_test_pp[[v]])
        
        df_test_pp <- predict(pre_proc, df_test_pp[feats])
        df_test_pp$Class <- teste$Class
        
        # Grid reduzido inspirado no "grid de ouro"
        C_vals <- c(0.25, 0.5, 1, 2)
        gam    <- c(0.5, 0.1, 0.01, 0.001)
        grid <- expand.grid(C = C_vals, sigma = gam)
        
        modelo_svm <- caret::train(
          Class ~ ., data = df_train_pp,
          method = "svmRadial",
          metric = "Accuracy",
          trControl = caret::trainControl(method = "cv", number = 3, savePredictions = TRUE),
          tuneGrid = grid
        )
        
        list(modelo = modelo_svm, df_test_pp = df_test_pp)
      },
      
      # ---------------- REDE NEURAL ----------------
      "ann" = caret::train(
        Class ~ ., data = treino,
        method = "nnet",
        trControl = ctrl,
        tuneLength = 3,
        trace = FALSE
      ),
      
      stop("Método desconhecido.")
    )
    
    # ===================== PREVISÃO ============================
    if (metodo == "svm") {
      prev <- predict(modelo$modelo, newdata = modelo$df_test_pp)
      real <- modelo$df_test_pp$Class
    } else {
      prev <- predict(modelo, newdata = teste)
      real <- teste$Class
    }
    
    prev <- factor(prev, levels = levels(df$Class))
    
    metricas <- calcular_metricas(real, prev)
    
    # Acurácia do treino
    acc_treino <- tryCatch({
      if (metodo == "svm") max(modelo$modelo$results$Accuracy)
      else max(modelo$results$Accuracy)
    }, error = function(e) NA)
    
    metricas$AcuraciaTreino <- acc_treino
    metricas$TempoSeg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    metricas$Modelo <- metodo
    metricas$Base   <- nome_base
    
    metricas
    
  }, error = function(e) {
    
    warning(paste("⚠️ Erro ao treinar modelo", metodo, ":", e$message))
    
    tibble(
      Acuracia = NA, Precisao = NA, Recall = NA, F1 = NA,
      TempoSeg = 0, Modelo = metodo, Base = nome_base,
      AcuraciaTreino = NA
    )
  })
  
  return(resultado)
}

# ------------------------------------------------------------
# Treinar todos os modelos
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
# Função principal
# ------------------------------------------------------------
treinar_em_lista <- function(lista_bases, cl = NULL) {
  
  own_cluster <- is.null(cl)
  if (own_cluster) cl <- iniciar_cluster()
  on.exit(if (own_cluster) finalizar_cluster(cl), add = TRUE)
  
  parallel::clusterExport(
    cl,
    varlist = c(
      "treinar_todos_modelos",
      "treinar_modelo",
      "calcular_metricas",
      "checar_predictors"
    ),
    envir = environment()
  )
  
  clusterEvalQ(cl, {
    library(C50)
    library(caret)
    library(kernlab)
    library(rpart)
    library(ranger)
    library(e1071)
    library(nnet)
    library(dplyr)
    library(LiblineaR)
  })
  
  resultados <- foreach(
    nome_base = names(lista_bases),
    .combine = bind_rows,
    .packages = c("caret", "rpart", "ranger", "e1071", "LiblineaR", "dplyr", "nnet", "kernlab")
  ) %dopar% {
    
    df <- lista_bases[[nome_base]]
    res <- treinar_todos_modelos(df, nome_base = nome_base)
    cat(paste0("✅ Concluído: ", nome_base, "\n"))
    res
  }
  
  cat("✅ Treinamento paralelo concluído!\n")
  
  log_tempo <- resultados %>%
    group_by(Base, Modelo) %>%
    summarise(
      TempoMedio = mean(TempoSeg, na.rm = TRUE),
      .groups = "drop"
    )
  
  dir.create("resultados/metricas", recursive = TRUE, showWarnings = FALSE)
  
  readr::write_csv(log_tempo,
                   file.path("resultados/metricas", "tempo_execucao_modelos.csv"))
  
  acuracia_treino <- resultados %>%
    select(Base, Modelo, AcuraciaTreino)
  
  readr::write_csv(acuracia_treino,
                   file.path("resultados/metricas", "acuracia_treinamento_modelos.csv"))
  
  return(resultados)
}