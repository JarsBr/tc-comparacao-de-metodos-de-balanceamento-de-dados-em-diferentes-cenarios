# ==========================================
# Script: 06_estatisticas_descritivas.R
# Objetivo: Calcular média, desvio padrão, mínimo e máximo
#           de cada variável em cada base, por método de balanceamento
# Entrada:  data/processed/lista_bases_[raw|oversampling|undersampling|smote|rose].RData
# Saída:    resultados/estatisticas/ (uma tabela CSV por base por método)
# ==========================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)

dir_processed <- "data/processed/"
dir_saida     <- "resultados/estatisticas/"

if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

metodos <- list(
  raw          = "lista_bases_raw.RData",
  oversampling = "lista_bases_oversampling.RData",
  undersampling= "lista_bases_undersampling.RData",
  smote        = "lista_bases_smote.RData",
  rose         = "lista_bases_rose.RData"
)

calcular_estatisticas <- function(df) {
  numericas <- df %>% select(where(is.numeric))

  if (ncol(numericas) == 0) {
    warning("Nenhuma variável numérica encontrada.")
    return(NULL)
  }

  bind_rows(
    numericas %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
      mutate(Estatistica = "Media"),
    numericas %>% summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) %>%
      mutate(Estatistica = "Desvio_Padrao"),
    numericas %>% summarise(across(everything(), ~ min(.x, na.rm = TRUE))) %>%
      mutate(Estatistica = "Minimo"),
    numericas %>% summarise(across(everything(), ~ max(.x, na.rm = TRUE))) %>%
      mutate(Estatistica = "Maximo")
  ) %>%
    relocate(Estatistica)
}

todas_estatisticas <- list()

for (nome_metodo in names(metodos)) {
  arquivo <- file.path(dir_processed, metodos[[nome_metodo]])

  if (!file.exists(arquivo)) {
    cat("⚠  Arquivo não encontrado, pulando:", arquivo, "\n")
    next
  }

  cat("\n========================================\n")
  cat("Método:", toupper(nome_metodo), "\n")
  cat("========================================\n")

  # Carrega o objeto (lista_bases_*)
  env_tmp <- new.env()
  load(arquivo, envir = env_tmp)
  lista_bases <- get(ls(env_tmp)[1], envir = env_tmp)

  for (nome_base in names(lista_bases)) {
    df <- lista_bases[[nome_base]]

    cat("  Base:", nome_base, "| linhas:", nrow(df), "| colunas:", ncol(df), "\n")

    estat <- calcular_estatisticas(df)

    if (is.null(estat)) next

    nome_arquivo <- paste0(nome_metodo, "_", gsub("[^[:alnum:]]", "_", nome_base), ".csv")
    write_csv(estat, file.path(dir_saida, nome_arquivo))

    todas_estatisticas[[paste(nome_metodo, nome_base, sep = "|")]] <- estat %>%
      mutate(Metodo = toupper(nome_metodo), Base = nome_base) %>%
      relocate(Metodo, Base)
  }
}

cat("\n\nGerando tabela consolidada...\n")

consolidado <- bind_rows(todas_estatisticas)

# Formato longo: uma linha por (Metodo, Base, Variavel, Estatistica)
consolidado_longo <- consolidado %>%
  pivot_longer(
    cols      = -c(Metodo, Base, Estatistica),
    names_to  = "Variavel",
    values_to = "Valor"
  )

write_csv(consolidado_longo, file.path(dir_saida, "estatisticas_consolidadas_longo.csv"))

# Formato largo: colunas Media, Desvio_Padrao, Minimo, Maximo
consolidado_largo <- consolidado_longo %>%
  pivot_wider(names_from = Estatistica, values_from = Valor)

write_csv(consolidado_largo, file.path(dir_saida, "estatisticas_consolidadas.csv"))

cat("Arquivos salvos em:", dir_saida, "\n")
cat("  - Um CSV por base/método\n")
cat("  - estatisticas_consolidadas.csv (formato largo)\n")
cat("  - estatisticas_consolidadas_longo.csv (formato longo)\n")
