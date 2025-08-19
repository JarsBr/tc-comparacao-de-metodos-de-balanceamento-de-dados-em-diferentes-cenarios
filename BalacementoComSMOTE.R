smote_data_balancer <- function(data) {
  # Instalar pacotes se necessário
  if (!require(smotefamily)) install.packages("smotefamily", dependencies=TRUE)
  if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
  if (!require(caret)) install.packages("caret", dependencies=TRUE)
  
  library(smotefamily)
  library(dplyr)
  library(caret)
  
  if (is.factor(data$Revenue)) {
    data$Revenue <- as.numeric(data$Revenue) - 1  
  } else {
    data$Revenue <- ifelse(data$Revenue == "TRUE", 1, 0)  
  }
  
  dummy_model <- dummyVars(" ~ .", data = data)
  data_numeric <- predict(dummy_model, newdata = data) %>% as.data.frame()
  
  X <- data_numeric %>% select(-Revenue)
  y <- data_numeric$Revenue
  
  smote_result <- tryCatch({
    SMOTE(X, y, K = 5, dup_size = 4)
  }, error = function(e) {
    print("Erro na merda do SMOTE.")
    return(data_numeric)
  })
  
  if (!"data" %in% names(smote_result)) {
    return(data_numeric)
  }
  
  data_balanced <- as.data.frame(smote_result$data)
  
  colnames(data_balanced)[ncol(data_balanced)] <- "Revenue"
  
  data_balanced$Revenue <- as.factor(ifelse(data_balanced$Revenue == 1, "TRUE", "FALSE"))
  
  return(data_balanced)
}
