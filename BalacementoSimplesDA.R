balance_data_augmentation <- function(data, target_col, sd_factor = 0.1) {
  if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
  
  target_levels <- unique(data[[target_col]])
  class_counts <- table(data[[target_col]])
  
  
  minority_class <- names(class_counts)[which.min(class_counts)]
  majority_class <- names(class_counts)[which.max(class_counts)]
  
  data_minority <- data %>% filter(.data[[target_col]] == minority_class)
  data_majority <- data %>% filter(.data[[target_col]] == majority_class)
  
  num_to_generate <- nrow(data_majority) - nrow(data_minority)
  
  if (num_to_generate <= 0) {
    return(data)
  }
  
  # Gerador de Ruido para evitar overfitting
  augmented_samples <- data_minority %>%
    slice_sample(n = num_to_generate, replace = TRUE) %>% 
    mutate(across(where(is.numeric), ~ . + rnorm(n(), mean = 0, sd = sd_factor * sd(.)))) 
  
  data_balanced <- bind_rows(data, augmented_samples)
  
  return(data_balanced)
}
