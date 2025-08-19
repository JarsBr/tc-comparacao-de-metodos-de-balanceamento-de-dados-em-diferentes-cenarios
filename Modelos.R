# Função SVM-RFE
# Sempre que usar  
treinar_svm_rfe <- function(dados_treino, dados_teste) {
  if (!require(caret)) install.packages("caret", dependencies=TRUE)
  if (!require(e1071)) install.packages("e1071", dependencies=TRUE)
  library(caret)
  library(e1071)
  
  ctrl <- rfeControl(functions = caretFuncs, method = "cv", number = 5)
  
  rfe_result <- rfe(x = dados_treino[, -ncol(dados_treino)], 
                    y = dados_treino$Revenue, 
                    sizes = c(2),  
                    rfeControl = ctrl)
  
  atributos_selecionados <- predictors(rfe_result)
  
  dados_treino_rfe <- dados_treino[, c(atributos_selecionados, "Revenue")]
  dados_teste_rfe <- dados_teste[, c(atributos_selecionados, "Revenue")]
  
  modelo_svm_rfe <- svm(Revenue ~ ., data = dados_treino_rfe, kernel = "linear",
                    cost = 1, scale = FALSE)
  
  previsoes <- predict(modelo_svm_rfe, newdata = dados_teste_rfe)
  
  matriz_confusao <- confusionMatrix(previsoes, dados_teste_rfe$Revenue)
  
  return(list(modelo = modelo_svm_rfe, atributos = atributos_selecionados, avaliacao = matriz_confusao))
}


# Função Random Forest
treinar_random_forest <- function(dados_treino, dados_teste) {
  if (!require(randomForest)) install.packages("randomForest", dependencies=TRUE)
  library(randomForest)
  
  modelo_rf <- randomForest(Revenue ~ ., data = dados_treino, ntree = 100,
                            mtry = sqrt(ncol(dados_treino) - 1),
                            importance = TRUE)
  
  previsoes <- predict(modelo_rf, newdata = dados_teste)

  matriz_confusao <- confusionMatrix(previsoes, dados_teste$Revenue)
  
  importancia_variaveis <- importance(modelo_rf)
  
  return(list(modelo = modelo_rf, avaliacao = matriz_confusao,
              importancia = importancia_variaveis))
}

# Função para treinar LightGBM
treinar_lightgbm <- function(dados_treino, dados_teste) {
  if (!require(lightgbm)) install.packages("lightgbm", dependencies=TRUE)
  library(lightgbm)
  
  
  dtrain <- lgb.Dataset(data = as.matrix(dados_treino[, -which(names(dados_treino) == "Revenue")]), label = as.numeric(dados_treino$Revenue) - 1)
  dtest <- as.matrix(dados_teste[, -which(names(dados_teste) == "Revenue")])
  label_test <- as.numeric(dados_teste$Revenue) - 1
  
  params <- list(
    objective = "binary", 
    metric = "binary_error", 
    learning_rate = 0.1, 
    max_depth = 6, 
    num_leaves = 31,
    num_iterations = 100
  )
  
  modelo_lgb <- lgb.train(params = params, data = dtrain, nrounds = 100)
  
  previsoes_prob <- predict(modelo_lgb, dtest)
  
  previsoes <- ifelse(previsoes_prob > 0.5, 1, 0)
  previsoes <- factor(previsoes, levels = c(0, 1))
  
  matriz_confusao <- confusionMatrix(previsoes, factor(label_test, levels = c(0, 1)))
  
  return(list(modelo = modelo_lgb, avaliacao = matriz_confusao))
}

