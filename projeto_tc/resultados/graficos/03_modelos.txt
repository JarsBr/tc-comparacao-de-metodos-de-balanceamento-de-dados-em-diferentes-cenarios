# ============================================================
# Script: 03_modelos.R
# Objetivo: Treinar e avaliar os modelos de classificação
# Modelos: Regressão Logística, Árvore (C5.0), Random Forest,
#          SVM (Radial) e Rede Neural (nnet)
# Entrada: Bases balanceadas e pré-processadas em data/processed/
# Saída:   Métricas consolidadas por modelo e por base, além de
#          arquivo com acurácia de treinamento e tempos de execução
# ============================================================

rm(list = ls())

library(caret)
library(C50)
library(rpart)
library(ranger)
library(e1071)
library(nnet)
library(dplyr)
library(foreach)
library(doParallel)
library(readr)

# ============================================================
# 1. MÉTRICAS
# ============================================================
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

# ============================================================
# 2. FUNÇÕES DE PRÉ-PROCESSAMENTO LEVE
# ============================================================

# Normalização global para modelos sensíveis à escala
preprocessar_modelos_lineares <- function(df) {
  feats <- setdiff(names(df), "Class")
  for (v in feats)
    if (is.factor(df[[v]])) df[[v]] <- as.numeric(df[[v]])
  
  pp <- caret::preProcess(df[feats], method = c("center", "scale", "medianImpute"))
  df_pp <- predict(pp, df[feats])
  df_pp$Class <- df$Class
  
  return(df_pp)
}

# Fixar classe positiva
fixar_classe_positiva <- function(df) {
  if (!"Class" %in% names(df)) stop("Class não encontrada!")
  
  # Ordem: classe majoritária no primeiro nível
  df$Class <- relevel(df$Class, ref = names(sort(table(df$Class), decreasing = TRUE))[1])
  return(df)
}

# ============================================================
# 3. TREINAMENTO POR MODELO
# ============================================================
treinar_modelo <- function(df, metodo, proporcao_treino = 0.7, nome_base = "desconhecida") {
  
  df <- fixar_classe_positiva(df)
  
  set.seed(123)
  idx <- caret::createDataPartition(df$Class, p = proporcao_treino, list = FALSE)
  
  treino <- df[idx, ]
  teste  <- df[-idx, ]
  
  t0 <- Sys.time()
  
  resultado <- tryCatch({
    
    ctrl <- caret::trainControl(method = "cv", number = 3)
    
    # ============================================================
    # MODELOS
    # ============================================================
    modelo <- switch(
      metodo,
      
      # ------------------------------------------------------------
      "logistico" =
        caret::train(
          Class ~ ., data = preprocessar_modelos_lineares(treino),
          method = "glm",
          family = binomial,
          trControl = ctrl
        ),
      
      # ------------------------------------------------------------
      "arvore" =
        caret::train(
          Class ~ ., data = treino,
          method = "C5.0",
          trControl = ctrl,
          tuneGrid = expand.grid(
            trials = c(1, 10, 20, 30),
            model = "tree",
            winnow = TRUE
          )
        ),
      
      # ------------------------------------------------------------
      "rf" =
        caret::train(
          Class ~ ., data = treino,
          method = "ranger",
          trControl = ctrl,
          tuneLength = 3
        ),
      
      # ------------------------------------------------------------
      "svm" = {
        treino_pp <- preprocessar_modelos_lineares(treino)
        teste_pp  <- preprocessar_modelos_lineares(teste)
        
        C_vals <- c(0.25, 0.5, 1, 2)
        gam    <- c(0.5, 0.1, 0.01, 0.001)
        grid   <- expand.grid(C = C_vals, sigma = gam)
        
        modelo_svm <- caret::train(
          Class ~ ., data = treino_pp,
          method = "svmRadial",
          metric = "Accuracy",
          trControl = ctrl,
          tuneGrid = grid
        )
        
        list(modelo = modelo_svm, teste_pp = teste_pp)
      },
      
      # ------------------------------------------------------------
      "ann" =
        caret::train(
          Class ~ ., data = preprocessar_modelos_lineares(treino),
          method = "nnet",
          trControl = ctrl,
          tuneLength = 3,
          trace = FALSE
        ),
      
      stop("Método desconhecido.")
    )
    
    # ============================================================
    # PREVISÃO
    # ============================================================
    if (metodo == "svm") {
      prev <- predict(modelo$modelo, newdata = modelo$teste_pp)
      real <- modelo$teste_pp$Class
      acc_treino <- max(modelo$modelo$results$Accuracy)
    } else {
      prev <- predict(modelo, newdata = teste)
      real <- teste$Class
      acc_treino <- max(modelo$results$Accuracy)
    }
    
    prev <- factor(prev, levels = levels(df$Class))
    
    metricas <- calcular_metricas(real, prev)
    metricas$AcuraciaTreino <- acc_treino
    metricas$TempoSeg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    metricas$Modelo <- metodo
    metricas$Base <- nome_base
    
    metricas
    
  }, error = function(e) {
    warning(paste("Erro no modelo", metodo, ":", e$message))
    
    tibble(
      Acuracia = NA, Precisao = NA, Recall = NA, F1 = NA,
      TempoSeg = 0, Modelo = metodo, Base = nome_base,
      AcuraciaTreino = NA
    )
  })
  
  return(resultado)
}

# ============================================================
# 4. Treinar todos os modelos
# ============================================================
treinar_todos_modelos <- function(df, nome_base) {
  bind_rows(
    treinar_modelo(df, "logistico", nome_base = nome_base),
    treinar_modelo(df, "arvore",    nome_base = nome_base),
    treinar_modelo(df, "rf",        nome_base = nome_base),
    treinar_modelo(df, "svm",       nome_base = nome_base),
    treinar_modelo(df, "ann",       nome_base = nome_base)
  )
}

# ============================================================
# 5. Funções de CLUSTER
# ============================================================
iniciar_cluster <- function() {
  n <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n)
  registerDoParallel(cl)
  message("Cluster iniciado com ", n, " núcleos.")
  return(cl)
}

finalizar_cluster <- function(cl) {
  if (!is.null(cl)) {
    stopCluster(cl)
    registerDoSEQ()
    message("Cluster finalizado.")
  }
}

# ============================================================
# 6. Função principal
# ============================================================
treinar_em_lista <- function(lista_bases, cl = NULL) {
  
  own <- is.null(cl)
  if (own) cl <- iniciar_cluster()
  on.exit(if (own) finalizar_cluster(cl), add = TRUE)
  
  clusterExport(
    cl,
    varlist = c(
      "treinar_todos_modelos", "treinar_modelo",
      "calcular_metricas", "preprocessar_modelos_lineares",
      "fixar_classe_positiva"
    ),
    envir = environment()
  )
  
  clusterEvalQ(cl, {
    library(caret); library(C50)
    library(ranger); library(e1071)
    library(nnet);  library(dplyr)
  })
  
  resultados <- foreach(
    nome_base = names(lista_bases),
    .combine   = bind_rows,
    .packages  = c("caret", "C50", "ranger", "e1071", "nnet", "dplyr")
  ) %dopar% {
    treinar_todos_modelos(lista_bases[[nome_base]], nome_base)
  }
  
  dir.create("resultados/metricas", recursive = TRUE, showWarnings = FALSE)
  
  write_csv(resultados,
            "resultados/metricas/metricas_completas.csv")
  
  return(resultados)
}
