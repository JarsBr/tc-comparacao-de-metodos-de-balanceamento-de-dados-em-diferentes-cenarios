# ==========================================
# Script: 05_pca_visualizacoes.R (versão final)
# ==========================================

rm(list = ls())

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

dir_processed <- "data/processed/"
dir_pca <- "resultados/graficos/pca/"
if (!dir.exists(dir_pca)) dir.create(dir_pca, recursive = TRUE)

load(file.path(dir_processed, "lista_bases_raw.RData"))
load(file.path(dir_processed, "lista_bases_smote.RData"))
load(file.path(dir_processed, "lista_bases_rose.RData"))
load(file.path(dir_processed, "lista_bases_undersampling.RData"))
load(file.path(dir_processed, "lista_bases_oversampling.RData"))

listas <- list(
  RAW = lista_bases_raw,
  SMOTE = bases_smote,
  ROSE = bases_rose,
  UNDER = bases_under,
  OVER = bases_over
)

detectar_col_classe <- function(df) {
  candidatos <- c("Classe", "class", "Class", "target", "Target", "y", "label", "Label")
  cand <- candidatos[candidatos %in% names(df)]
  if (length(cand) == 0) stop("Nenhuma coluna de classe encontrada.")
  cand[1]
}

# ------------------------------------------------------------
# Função para limpar dados numéricos antes do PCA
# ------------------------------------------------------------
preparar_dados_pca <- function(df) {
  df_num <- df %>% select(where(is.numeric))
  
  # Substitui Inf / -Inf por NA
  df_num[sapply(df_num, is.infinite)] <- NA
  
  # Remove colunas completamente vazias (só NA/NaN)
  colunas_validas <- names(df_num)[colSums(!is.na(df_num)) > 0]
  df_num <- df_num[, colunas_validas, drop = FALSE]
  
  # Substitui NA/NaN pela média da coluna (com fallback = 0 se toda coluna for NA)
  df_num <- df_num %>%
    mutate(across(everything(), ~ {
      x <- .x
      x[is.infinite(x)] <- NA
      media <- mean(x, na.rm = TRUE)
      if (is.na(media)) media <- 0
      x[is.na(x) | is.nan(x)] <- media
      x
    }))
  
  # Remove colunas constantes (var = 0 ou NA)
  variancias <- sapply(df_num, function(x) var(x, na.rm = TRUE))
  variancias[is.na(variancias)] <- 0
  df_num <- df_num[, variancias > 0, drop = FALSE]
  
  df_num
}


gerar_pca_comparativo <- function(nome_base) {
  cat("\n📊 Gerando PCA facetado para base:", nome_base, "\n")
  
  dfs <- list()
  for (metodo in names(listas)) {
    lista_atual <- listas[[metodo]]
    if (!nome_base %in% names(lista_atual)) next
    df <- lista_atual[[nome_base]]
    col_class <- detectar_col_classe(df)
    df <- df %>% rename(Classe = all_of(col_class))
    df$Classe <- as.factor(df$Classe)
    df$Metodo <- metodo
    dfs[[metodo]] <- df
  }
  
  if (length(dfs) < 2) {
    warning(paste("Base", nome_base, "tem menos de 2 métodos disponíveis, ignorada."))
    return(NULL)
  }
  
  dados <- bind_rows(dfs)
  
  set.seed(123)
  dados_sample <- dados %>%
    group_by(Metodo, Classe) %>%
    group_modify(~ {
      n_amostrar <- min(1000, nrow(.x))
      .x %>% slice_sample(n = n_amostrar)
    }) %>%
    ungroup()
  
  # ------------------------------------------------------------
  # Limpa e prepara dados numéricos
  # ------------------------------------------------------------
  vars_num <- preparar_dados_pca(dados_sample)
  if (ncol(vars_num) < 2) {
    warning(paste("Base", nome_base, "tem poucas variáveis numéricas. Ignorada."))
    return(NULL)
  }
  
  # ------------------------------------------------------------
  # PCA
  # ------------------------------------------------------------
  if ("RAW" %in% dados_sample$Metodo) {
    pca_raw <- FactoMineR::PCA(
      preparar_dados_pca(dados_sample %>% filter(Metodo == "RAW")),
      graph = FALSE, scale.unit = TRUE
    )
    
    proj <- bind_rows(lapply(split(dados_sample, dados_sample$Metodo), function(df) {
      coords <- predict(pca_raw, preparar_dados_pca(df))
      coords_df <- as.data.frame(coords)
      if (!all(c("Dim.1", "Dim.2") %in% names(coords_df))) {
        names(coords_df)[1:2] <- c("Dim.1", "Dim.2")
      }
      df$Dim.1 <- coords_df$Dim.1
      df$Dim.2 <- coords_df$Dim.2
      df
    }))
    
  } else {
    pca_all <- FactoMineR::PCA(vars_num, graph = FALSE, scale.unit = TRUE)
    coords <- as.data.frame(pca_all$ind$coord[, 1:2])
    names(coords) <- c("Dim.1", "Dim.2")
    proj <- bind_cols(dados_sample, coords)
  }
  
  # ------------------------------------------------------------
  # Gráfico facetado
  # ------------------------------------------------------------
  grafico <- ggplot(proj, aes(Dim.1, Dim.2, color = Classe, fill = Classe)) +
    geom_point(alpha = 0.3, size = 1) +
    stat_ellipse(level = 0.95, linewidth = 0.7, alpha = 0.6) +
    facet_wrap(~Metodo, ncol = 2) +
    labs(
      title = paste("PCA Comparativo -", nome_base),
      x = "Componente Principal 1",
      y = "Componente Principal 2"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom"
    )
  
  caminho <- file.path(dir_pca, paste0("PCA_Facet_", nome_base, ".png"))
  ggsave(caminho, grafico, width = 10, height = 8)
  cat("✅ PCA facetado salvo em:", caminho, "\n")
}

# ------------------------------------------------------------
# Executa PCA comparativo para todas as bases
# ------------------------------------------------------------
bases_disponiveis <- names(listas$RAW)
cat("\n🔍 Bases encontradas:", length(bases_disponiveis), "\n")

for (base in bases_disponiveis) {
  tryCatch(
    gerar_pca_comparativo(base),
    error = function(e) cat("⚠️ Erro em", base, ":", conditionMessage(e), "\n")
  )
}

cat("\n🎨 PCA facetado concluído! Resultados salvos em:", dir_pca, "\n")
