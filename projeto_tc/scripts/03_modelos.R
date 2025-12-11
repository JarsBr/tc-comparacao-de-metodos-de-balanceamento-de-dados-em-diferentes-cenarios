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
library(kernlab)   # usado pelo caret para svmRadial
library(stringr)

# ------------------------------------------------------------
# Métricas de desempenho
# ------------------------------------------------------------
calcular_metricas <- function(real, previsto) {
  cm <- caret::confusionMatrix(
    data = previsto,
    reference = real,
    positive = levels(real)[2]
  )
  
  # Extrair métricas (usar as posições com segurança)
  acc  <- as.numeric(cm$overall["Accuracy"])
  prec <- as.numeric(cm$byClass["Precision"])
  rec  <- as.numeric(cm$byClass["Recall"])
  f1   <- as.numeric(cm$byClass["F1"])
  
  # Correção: recall = 0 → precision = 0, f1 = 0
  if (is.na(rec) || rec == 0) {
    prec <- 0
    f1 <- 0
  }
  
  # Correção extra
  if (is.na(prec)) prec <- 0
  if (is.na(f1))   f1   <- 0
  
  tibble(
    Acuracia = acc,
    Precisao = prec,
    Recall   = rec,
    F1       = f1
  )
}

# ------------------------------------------------------------
# Função de pré-processamento UNIFICADO
# - dummyVars (one-hot, fullRank=TRUE)
# - preProcess: center, scale, medianImpute
# Retorna lista com x_train_pp, x_test_pp (data.frames com Class)
# ------------------------------------------------------------
preprocessar_unificado <- function(treino, teste, verbose = FALSE) {
  if (!"Class" %in% names(treino)) stop("Variável alvo 'Class' não encontrada no treino.")
  if (!"Class" %in% names(teste)) stop("Variável alvo 'Class' não encontrada no teste.")
  
  # 1) Dummy encoding (para evitar ordinalização de fatores)
  dv <- caret::dummyVars(Class ~ ., data = treino, fullRank = TRUE)
  x_train <- as.data.frame(predict(dv, newdata = treino))
  x_test  <- as.data.frame(predict(dv, newdata = teste))
  
  # 2) Preprocess (center/scale/impute)
  pre_proc <- caret::preProcess(x_train, method = c("center", "scale", "medianImpute"))
  x_train_pp <- predict(pre_proc, x_train)
  x_test_pp  <- predict(pre_proc, x_test)
  
  # Adiciona a coluna Class de volta
  x_train_pp$Class <- treino$Class
  x_test_pp$Class  <- teste$Class
  
  if (verbose) {
    message("  -> pré-processamento unificado aplicado: dummyVars + center/scale + medianImpute")
    message("     features treino:", ncol(x_train_pp) - 1, " / instâncias treino:", nrow(x_train_pp))
  }
  
  list(train = x_train_pp, test = x_test_pp, dv = dv, pre_proc = pre_proc)
}

# ------------------------------------------------------------
# Função: treinar e avaliar um modelo genérico (versão final)
# ------------------------------------------------------------
treinar_modelo <- function(df, metodo, proporcao_treino = 0.7, nome_base = "desconhecida", verbose = FALSE) {
  
  if (!"Class" %in% names(df)) stop("⚠️ Variável alvo 'Class' não encontrada.")
  
  # Partição estratificada (externa)
  set.seed(123)
  idx <- caret::createDataPartition(df$Class, p = proporcao_treino, list = FALSE)
  treino <- df[idx, ]
  teste  <- df[-idx, ]
  
  t0 <- Sys.time()
  
  resultado <- tryCatch({
    
    # 1) pré-processamento unificado (aplica dummy + scale + impute)
    pp <- preprocessar_unificado(treino, teste, verbose = verbose)
    x_train_pp <- pp$train
    x_test_pp  <- pp$test
    
    # remove Class para treinar com formula
    feats <- setdiff(names(x_train_pp), "Class")
    formula <- as.formula(paste("Class ~", paste(feats, collapse = " + ")))
    
    # Controle padrão (interno para tuning)
    ctrl <- caret::trainControl(method = "cv", number = 3, classProbs = FALSE)
    
    # =======================================
    #       TREINAMENTO DOS MODELOS
    # =======================================
    modelo <- switch(
      metodo,
      
      # ---------------- LOGÍSTICO ----------------
      "logistico" = caret::train(
        formula, data = x_train_pp,
        method = "glm",
        family = binomial,
        trControl = ctrl
      ),
      
      # ---------------- ÁRVORE (C5.0) ----------------
      "arvore" = caret::train(
        formula, data = x_train_pp,
        method = "C5.0",
        trControl = ctrl,
        tuneGrid = expand.grid(
          trials = c(1, 10, 20, 30),
          model = "tree",
          winnow = TRUE
        )
      ),
      
      # ---------------- RANDOM FOREST (ranger) ----------------
      "rf" = {
        p <- max(1, ncol(x_train_pp) - 1)
        # mtry candidates (dynamic, garante >=1)
        mtry_vals <- unique(pmax(1, floor(c(sqrt(p), p/3, p/2))))
        tune_rf <- expand.grid(
          mtry = mtry_vals,
          splitrule = "gini",
          min.node.size = c(1, 5)
        )
        caret::train(
          formula, data = x_train_pp,
          method = "ranger",
          trControl = ctrl,
          tuneGrid = tune_rf,
          importance = "impurity"
        )
      },
      
      # ---------------- SVM Radial (grid reduzido + 3-fold) ----------------
      "svm" = {
        train_contrl <- caret::trainControl(method = "cv", number = 3, savePredictions = TRUE)
        
        # Grid reduzido (mantém a filosofia do "grid ouro", mas enxuto)
        C_vals <- c(0.25, 0.5, 1, 2)
        gam <- c(0.5, 0.1, 0.01, 0.001)
        grid <- expand.grid(C = C_vals, sigma = gam)
        
        caret::train(
          formula, data = x_train_pp,
          method = "svmRadial",
          metric = "Accuracy",
          trControl = train_contrl,
          tuneGrid = grid
        )
      },
      
      # ---------------- REDE NEURAL (nnet) ----------------
      "ann" = {
        # grid razoável para nnet: size x decay
        tune_ann <- expand.grid(size = c(3, 5, 8), decay = c(0, 0.001, 0.01))
        caret::train(
          formula, data = x_train_pp,
          method = "nnet",
          trControl = ctrl,
          tuneGrid = tune_ann,
          trace = FALSE,
          maxit = 200
        )
      },
      
      stop("⚠️ Método desconhecido.")
    ) # end switch
    
    # =======================================
    #              PREDIÇÃO
    # =======================================
    prev <- predict(modelo, newdata = x_test_pp)
    real <- x_test_pp$Class
    
    # Garante níveis
    prev <- factor(prev, levels = levels(df$Class))
    
    # =======================================
    #             MÉTRICAS
    # =======================================
    metricas <- calcular_metricas(real, prev)
    
    # ACURÁCIA DE TREINAMENTO (se disponível)
    acc_treino <- NA_real_
    if (!is.null(modelo$results) && "Accuracy" %in% names(modelo$results)) {
      acc_treino <- max(modelo$results$Accuracy, na.rm = TRUE)
    }
    
    metricas$AcuraciaTreino <- acc_treino
    
    # TEMPO E IDENTIFICAÇÃO
    duracao <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    metricas$TempoSeg <- duracao
    metricas$Modelo <- metodo
    metricas$Base <- nome_base
    
    # Também retornar o objeto modelo (útil para debugging)
    attr(metricas, "modelo_obj") <- modelo
    
    if (verbose) {
      message(sprintf("  -> %s | Base: %s | Tempo: %.1f s | AccTest: %.4f | AccTrain: %s",
                      metodo, nome_base, duracao, metricas$Acuracia, ifelse(is.na(acc_treino), "NA", sprintf("%.4f", acc_treino))))
    }
    
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
# Função: treinar todos os modelos em uma base
# ------------------------------------------------------------
treinar_todos_modelos <- function(df, nome_base = "desconhecida", verbose = FALSE) {
  dplyr::bind_rows(
    treinar_modelo(df, "logistico", nome_base = nome_base, verbose = verbose),
    treinar_modelo(df, "arvore",    nome_base = nome_base, verbose = verbose),
    treinar_modelo(df, "rf",        nome_base = nome_base, verbose = verbose),
    treinar_modelo(df, "svm",       nome_base = nome_base, verbose = verbose),
    treinar_modelo(df, "ann",       nome_base = nome_base, verbose = verbose)
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
treinar_em_lista <- function(lista_bases, cl = NULL, verbose = FALSE) {
  
  own_cluster <- is.null(cl)
  if (own_cluster) cl <- iniciar_cluster()
  on.exit(if (own_cluster) finalizar_cluster(cl), add = TRUE)
  
  # Exporta funções e objetos necessários
  parallel::clusterExport(
    cl,
    varlist = c(
      "treinar_todos_modelos",
      "treinar_modelo",
      "calcular_metricas",
      "preprocessar_unificado"
    ),
    envir = environment()
  )
  
  # Carregar pacotes necessários dentro de cada nó do cluster
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
  
  # Execução paralela para cada base
  resultados <- foreach(
    nome_base = names(lista_bases),
    .combine = bind_rows,
    .packages = c(
      "caret", "rpart", "ranger", "e1071",
      "LiblineaR", "dplyr", "nnet", "kernlab"
    )
  ) %dopar% {
    
    df <- lista_bases[[nome_base]]
    
    res <- treinar_todos_modelos(df, nome_base = nome_base, verbose = verbose)
    
    cat(paste0("✅ Concluído: ", nome_base, "\n"))
    
    res
  }
  
  cat("✅ Treinamento paralelo concluído!\n")
  
  # Salvar log de tempos
  log_tempo <- resultados %>%
    group_by(Base, Modelo) %>%
    summarise(
      TempoMedio = mean(TempoSeg, na.rm = TRUE),
      .groups = "drop"
    )
  
  dir.create("resultados/metricas", recursive = TRUE, showWarnings = FALSE)
  
  readr::write_csv(
    log_tempo,
    file.path("resultados/metricas", "tempo_execucao_modelos.csv")
  )
  
  # Salvar acurácia de treinamento
  acuracia_treino <- resultados %>%
    select(Base, Modelo, AcuraciaTreino)
  
  readr::write_csv(
    acuracia_treino,
    file.path("resultados/metricas", "acuracia_treinamento_modelos.csv")
  )
  
  return(resultados)
}
