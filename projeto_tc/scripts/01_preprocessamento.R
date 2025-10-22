# ==========================================
# Script: 01_preprocessamento.R
# Objetivo: Ler, limpar e padronizar todas as bases de dados
# Estrutura: cada base está em uma subpasta dentro de data/raw/
# Suporte: arquivos .csv e .data (+ leitura de arquivos com nomes de colunas, alguns casos)
# Saída: objeto lista_bases_raw salvo em data/processed/
# ==========================================

rm(list = ls())

library(dplyr)
library(readr)
library(stringr)
library(tools)

dir_raw <- "data/raw/"
dir_processed <- "data/processed/"

# Função auxiliar: tentar detectar separador
detectar_sep <- function(caminho) {
  linhas <- readLines(caminho, n = 5)
  if (any(grepl(";", linhas))) return(";")
  else return(",")
}

# Função: tenta ler nomes de colunas de arquivos auxiliares
ler_nomes_colunas <- function(pasta) {
  arquivos_nomes <- list.files(
    pasta, 
    pattern = "\\.(names|txt|columns?)$", 
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(arquivos_nomes) > 0) {
    cat("   ↳ Nomes de colunas encontrados em:", basename(arquivos_nomes[1]), "\n")
    nomes <- readLines(arquivos_nomes[1])
    nomes <- nomes[nchar(nomes) > 0]
    nomes <- str_trim(nomes)
    nomes <- str_replace_all(nomes, "[^[:alnum:]_]", "_")
    return(nomes)
  }
  return(NULL)
}

# Função: leitura genérica de arquivo .csv ou .data
ler_arquivo_generico <- function(caminho_arquivo) {
  ext <- file_ext(caminho_arquivo)
  sep <- detectar_sep(caminho_arquivo)
  
  if (ext %in% c("csv", "data")) {
    dados <- read_delim(caminho_arquivo, delim = sep, show_col_types = FALSE)
  } else {
    stop(paste("Formato de arquivo não suportado:", ext))
  }
  return(dados)
}

# Função principal: pré-processar uma base
preprocessar_base <- function(caminho_arquivo) {
  pasta <- dirname(caminho_arquivo)
  nome_base <- basename(pasta)
  cat(" Processando base:", nome_base, "\n")
  
  # Le dados
  dados <- ler_arquivo_generico(caminho_arquivo)
  
  # Se existir arquivo de nomes de colunas, substitui
  nomes_colunas <- ler_nomes_colunas(pasta)
  if (!is.null(nomes_colunas) && length(nomes_colunas) == ncol(dados)) {
    names(dados) <- nomes_colunas
  }
  
  # Padroniza nomes de colunas
  names(dados) <- tolower(names(dados))
  names(dados) <- str_replace_all(names(dados), "[^[:alnum:]_]", "_")
  
  # Renomeia variável alvo para 'Class'
  nomes_alvo_possiveis <- c("revenue", "target", "classe", "label", "output", "y", "class", "outcome")
  for (n in nomes_alvo_possiveis) {
    if (n %in% names(dados)) {
      names(dados)[names(dados) == n] <- "Class"
    }
  }
  
  # Remove colunas totalmente vazias -- Acredito nao ter nenhuma nas nossa base, mas deixei por garantia
  dados <- dados[, colSums(is.na(dados)) < nrow(dados)]
  
  # Preenche NAs numéricos com mediana-- Acredito nao ter nenhuma nas nossa base, mas deixei por garantia
  dados <- dados %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  
  # Converte a classe para fator
  if ("Class" %in% names(dados)) {
    dados$Class <- as.factor(dados$Class)
  } else {
    warning(paste("A base", nome_base, "não contém variável alvo identificável."))
  }
  
  return(dados)
}

# Identifica subpastas de bases
subpastas <- list.dirs(dir_raw, recursive = FALSE)

lista_bases_raw <- list()

for (pasta in subpastas) {
  arquivos <- list.files(pasta, pattern = "\\.(csv|data)$", full.names = TRUE)
  
  if (length(arquivos) == 0) {
    warning(paste("⚠️ Nenhum arquivo de dados encontrado em:", pasta))
    next
  }
  
  dados <- preprocessar_base(arquivos[1])
  lista_bases_raw[[basename(pasta)]] <- dados
}

# Cria pasta de saída e salva a lista consolidada
if (!dir.exists(dir_processed)) dir.create(dir_processed, recursive = TRUE) # Coloquei pois estava apagando a pasta para rodar de novo
save(lista_bases_raw, file = file.path(dir_processed, "lista_bases_raw.RData"))

cat("\n✅ Pré-processamento concluído!")
cat("\n Lista salva em data/processed/lista_bases_raw.RData\n")
