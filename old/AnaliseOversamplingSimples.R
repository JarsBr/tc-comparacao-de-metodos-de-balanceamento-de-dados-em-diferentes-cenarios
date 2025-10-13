if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
if (!require(caret)) install.packages("caret", dependencies=TRUE)

dados <- read.csv("dataset/online_shoppers_intention.csv")

dados$Month <- factor(dados$Month)
dados$VisitorType <- factor(dados$VisitorType)
dados$Weekend <- as.factor(dados$Weekend)
dados$Revenue <- as.factor(dados$Revenue)

variaveis_continuas <- c("Administrative_Duration", "Informational_Duration", 
                         "ProductRelated_Duration", "BounceRates", "ExitRates")
dados[variaveis_continuas] <- scale(dados[variaveis_continuas])

data_balanced <- balance_data_augmentation(dados, target_col = "Revenue", sd_factor = 0.1)

set.seed(0)
indice_treino <- createDataPartition(data_balanced$Revenue, p = 0.7, list = FALSE)
dados_treino <- data_balanced[indice_treino, ]
dados_teste <- data_balanced[-indice_treino, ]

# Treinando e avaliando os modelos:

# # # Random Forest - 
print("Treinando Modelo Random Forest:")
resultado_rf <- treinar_random_forest(dados_treino, dados_teste)
print("Resultados Random Forest:")
print(resultado_rf$avaliacao)
print(resultado_rf$importancia)

# # # LightGBM - 
print("Treinando Modelo LightGBM:")
resultado_lgb <- treinar_lightgbm(dados_treino, dados_teste)
print("Resultados LightGBM:")
print(resultado_lgb$avaliacao)

# # # SVM-RFE 
print("Treinando Modelo SVM-RFE:")
resultado_svm <- treinar_svm_rfe(dados_treino, dados_teste)
print("Resultados SVM:")
print(resultado_svm$avaliacao)