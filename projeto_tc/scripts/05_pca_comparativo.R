# ==========================================
# Script: 05_pca_visualizacoes.R (final)
# ==========================================

rm(list = ls())

library(FactoMineR)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(patchwork)


# ------------------------------------------------------------
# Diretórios
# ------------------------------------------------------------
dir_processed <- "data/processed/"
dir_pca <- "resultados/graficos/pca/"
if (!dir.exists(dir_pca)) dir.create(dir_pca, recursive = TRUE)

# ------------------------------------------------------------
# Carrega bases
# ------------------------------------------------------------
load(file.path(dir_processed, "lista_bases_raw.RData"))
load(file.path(dir_processed, "lista_bases_smote.RData"))
load(file.path(dir_processed, "lista_bases_rose.RData"))
load(file.path(dir_processed, "lista_bases_undersampling.RData"))
load(file.path(dir_processed, "lista_bases_oversampling.RData"))

listas <- list(
  RAW   = lista_bases_raw,
  OVER  = bases_over,
  UNDER = bases_under,
  SMOTE = bases_smote,
  ROSE  = bases_rose
)

# ------------------------------------------------------------
# Funções auxiliares
# ------------------------------------------------------------

padronizar_nomes_colunas <- function(df) {
  nomes <- colnames(df)
  nomes <- gsub("^X([0-9]+)$", "\\1", nomes)
  colnames(df) <- nomes
  df
}

detectar_col_classe <- function(df) {
  candidatos <- c("Classe", "class", "Class", "target", "Target", "y", "label", "Label")
  cand <- candidatos[candidatos %in% names(df)]
  if (length(cand) == 0) stop("Nenhuma coluna de classe encontrada.")
  cand[1]
}

preparar_dados_pca <- function(df) {
  df_num <- df %>% select(where(is.numeric))
  
  df_num[sapply(df_num, is.infinite)] <- NA
  
  colunas_validas <- names(df_num)[colSums(!is.na(df_num)) > 0]
  df_num <- df_num[, colunas_validas, drop = FALSE]
  
  df_num <- df_num %>%
    mutate(across(everything(), ~ {
      x <- .x
      media <- mean(x, na.rm = TRUE)
      if (is.na(media)) media <- 0
      x[is.na(x) | is.nan(x)] <- media
      x
    }))
  
  variancias <- sapply(df_num, function(x) var(x, na.rm = TRUE))
  variancias[is.na(variancias)] <- 0
  df_num <- df_num[, variancias > 0, drop = FALSE]
  
  df_num
}

# ------------------------------------------------------------
# Função de plot individual
# ------------------------------------------------------------
plot_metodo <- function(df, titulo = NULL, 
                        var_exp_total = NULL,
                        mostrar_legenda = FALSE) {
  
  # Monta título com variância explicada
  if (!is.null(var_exp_total)) {
    titulo_final <- paste0(
      titulo, 
      " (", 
      format(round(var_exp_total, 2), nsmall = 2), 
      "% da variância explicada)"
    )
  } else {
    titulo_final <- titulo
  }
  
  ggplot(df, aes(Dim.1, Dim.2, color = Classe, fill = Classe)) +
    geom_point(alpha = 0.3, size = 1) +
    stat_ellipse(level = 0.95, linewidth = 0.7, alpha = 0.6) +
    labs(
      title = titulo_final,
      x = "Componente Principal 1",
      y = "Componente Principal 2"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = if (mostrar_legenda) "bottom" else "none"
    )
}

# ------------------------------------------------------------
# PCA comparativo por base
# ------------------------------------------------------------
gerar_pca_comparativo <- function(nome_base) {
  
  cat("\n📊 Gerando PCA para base:", nome_base, "\n")
  
  dfs <- list()
  
  for (metodo in names(listas)) {
    lista_atual <- listas[[metodo]]
    if (!nome_base %in% names(lista_atual)) next
    
    df <- lista_atual[[nome_base]]
    df <- padronizar_nomes_colunas(df)
    
    col_class <- detectar_col_classe(df)
    
    df <- df %>%
      rename(Classe = all_of(col_class)) %>%
      mutate(
        Classe = as.factor(Classe),
        Metodo = metodo
      )
    
    dfs[[metodo]] <- df
  }
  
  if (length(dfs) < 2) {
    warning(paste("Base", nome_base, "tem menos de 2 métodos disponíveis. Ignorada."))
    return(NULL)
  }
  
  dados <- bind_rows(dfs)
  
  # ------------------------------------------------------------
  # Amostragem para visualização
  # ------------------------------------------------------------
  set.seed(123)
  
  dados_sample <- dados %>%
    group_by(Metodo, Classe) %>%
    group_modify(~ {
      n_amostrar <- min(1000, nrow(.x))
      slice_sample(.x, n = n_amostrar)
    }) %>%
    ungroup()
  
  
  # ------------------------------------------------------------
  # PCA ajustado somente no RAW
  # ------------------------------------------------------------
  dados_raw <- dados_sample %>% filter(Metodo == "RAW")
  vars_raw <- preparar_dados_pca(dados_raw)
  
  if (ncol(vars_raw) < 2) {
    warning(paste("Base", nome_base, "tem poucas variáveis numéricas no RAW."))
    return(NULL)
  }
  
  pca_raw <- PCA(vars_raw, graph = FALSE, scale.unit = TRUE)
  # Variância explicada pelos dois primeiros componentes
  eig_vals <- pca_raw$eig
  var_exp_total <- eig_vals[1, 2] + eig_vals[2, 2]
  vars_pca <- colnames(vars_raw)
  
  # ------------------------------------------------------------
  # Projeção de todos os métodos
  # ------------------------------------------------------------
  proj <- bind_rows(lapply(split(dados_sample, dados_sample$Metodo), function(df) {
    
    df_num <- preparar_dados_pca(df)
    
    cols_faltantes <- setdiff(vars_pca, colnames(df_num))
    if (length(cols_faltantes) > 0) {
      for (c in cols_faltantes) df_num[[c]] <- 0
    }
    
    df_num <- df_num[, vars_pca, drop = FALSE]
    
    coords <- predict(pca_raw, df_num)
    
    if (is.list(coords) && "coord" %in% names(coords)) {
      coords_df <- as.data.frame(coords$coord[, 1:2])
    } else {
      coords_df <- as.data.frame(coords[, 1:2])
    }
    
    colnames(coords_df) <- c("Dim.1", "Dim.2")
    
    bind_cols(df, coords_df)
  }))
  
  # ------------------------------------------------------------
  # Cria gráficos individuais
  # ------------------------------------------------------------
  p_raw   <- plot_metodo(filter(proj, Metodo == "RAW"),   "RAW",
                         var_exp_total, TRUE)
  
  p_over  <- plot_metodo(filter(proj, Metodo == "OVER"),  "OVER",
                         var_exp_total)
  
  p_under <- plot_metodo(filter(proj, Metodo == "UNDER"), "UNDER",
                         var_exp_total)
  
  p_smote <- plot_metodo(filter(proj, Metodo == "SMOTE"), "SMOTE",
                         var_exp_total)
  
  p_rose  <- plot_metodo(filter(proj, Metodo == "ROSE"),  "ROSE",
                         var_exp_total)
  
  # ------------------------------------------------------------
  # Layout final (RAW sozinho + 2x2 abaixo)
  # ------------------------------------------------------------
  grafico <-
    (p_raw | plot_spacer()) / 
    (p_over | p_under) /        
    (p_smote | p_rose)         
  
  # ------------------------------------------------------------
  # Salvar
  # ------------------------------------------------------------
  caminho <- file.path(dir_pca, paste0("PCA_Facet_", nome_base, ".png"))
  ggsave(caminho, grafico, width = 10, height = 12)
  
  cat("✅ PCA salvo em:", caminho, "\n")
}

# ------------------------------------------------------------
# Executa para todas as bases
# ------------------------------------------------------------
bases_disponiveis <- names(listas$RAW)
cat("\n🔍 Bases encontradas:", length(bases_disponiveis), "\n")

for (base in bases_disponiveis) {
  tryCatch(
    gerar_pca_comparativo(base),
    error = function(e)
      cat("⚠️ Erro em", base, ":", conditionMessage(e), "\n")
  )
}

cat("\n🎨 PCA concluído! Resultados em:", dir_pca, "\n")
