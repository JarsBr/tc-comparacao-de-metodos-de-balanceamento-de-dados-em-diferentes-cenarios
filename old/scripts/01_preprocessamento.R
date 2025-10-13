# ==========================================
# Script: 01_preprocessamento.R
# Objetivo: Ler, limpar e padronizar todas as bases de dados
# Saída: objeto lista_bases_raw salvo em data/processed/
# ==========================================

# Limpa o ambiente
rm(list = ls())

# Pacotes necessários
library(dplyr)
library(readr)
library(stringr)

# Caminhos
dir_raw <- "data/raw/"
dir_processed <- "data/processed/"

# Lista de arquivos .csv na pasta raw
arquivos <- list.files(dir_raw, pattern = "\\.csv$", full.names = TRUE)

# Função para padronizar e limpar cada base ----
preprocessar_base <- function(caminho_arquivo) {
  # Lê o arquivo
  dados <- read_csv(caminho_arquivo, show_col_types = FALSE)
  
  # Nome do arquivo (sem extensão)
  nome_base <- tools::file_path_sans_ext(basename(caminho_arquivo))
  
  cat("Processando base:", nome_base, "\n")
  
  # Converte nomes de colunas para minúsculas
  names(dados) <- tolower(names(dados))
  
  # Remove espaços e caracteres especiais dos nomes
  names(dados) <- str_replace_all(names(dados), "[^[:alnum:]_]", "_")
  
  # Renomeia a variável alvo para 'Class'
  # (ajuste conforme o nome da variável alvo nas suas bases)
  nomes_alvo_possiveis <- c("revenue", "target", "classe", "label", "output")
  for (n in nomes_alvo_possiveis) {
    if (n %in% names(dados)) {
      names(dados)[names(dados) == n] <- "Class"
    }
  }
  
  # Remove colunas totalmente vazias
  dados <- dados[, colSums(is.na(dados)) < nrow(dados)]
  
  # Trata NAs simples (exemplo: substitui por mediana)
  dados <- dados %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  
  # Garante que Class seja fator
  if ("Class" %in% names(dados)) {
    dados$Class <- as.factor(dados$Class)
  } else {
    warning(paste("A base", nome_base, "não contém variável alvo identificável."))
  }
  
  return(dados)
}

# Aplica a função a todas as bases ----
lista_bases_raw <- lapply(arquivos, preprocessar_base)
names(lista_bases_raw) <- tools::file_path_sans_ext(basename(arquivos))

# Salva a lista em arquivo RData ----
if (!dir.exists(dir_processed)) dir.create(dir_processed, recursive = TRUE)
save(lista_bases_raw, file = file.path(dir_processed, "lista_bases_raw.RData"))

cat("✅ Pré-processamento concluído e lista salva em data/processed/lista_bases_raw.RData\n")
