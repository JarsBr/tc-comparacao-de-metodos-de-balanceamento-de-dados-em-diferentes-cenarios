rose_data_balancer <- function(data) {
  if (!require(ROSE)) install.packages("ROSE", dependencies=TRUE)
  library(ROSE)
  
  data$Revenue <- as.factor(data$Revenue)
  data$Month <- as.factor(data$Month)
  data$VisitorType <- as.factor(data$VisitorType)
  data$Weekend <- as.factor(data$Weekend)
  
  data_balanced <- ROSE(Revenue ~ ., data = data, seed = 0)$data
  
  return(data_balanced)
}
