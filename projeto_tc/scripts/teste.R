# ==========================================
# Diagnóstico: contagem de valores ausentes por base (versão robusta)
# ==========================================

library(dplyr)
library(purrr)

# Lista geral com todas as técnicas (ajuste os nomes se necessário)
listas <- list(
  RAW = lista_bases_raw,
  SMOTE = bases_smote,
  ROSE = bases_rose,
  UNDER = bases_under,
  OVER = bases_over
)

# Função auxiliar para contar NAs, NaN e Inf de forma segura
contar_ausentes <- function(df) {
  # Mantém apenas colunas "simples" (numéricas, lógicas, caracteres, fatores)
  df_limpo <- df[, sapply(df, function(x) !is.list(x)), drop = FALSE]
  
  total_valores <- nrow(df_limpo) * ncol(df_limpo)
  if (total_valores == 0) {
    return(tibble(Linhas = nrow(df), Colunas = ncol(df), Ausentes = NA, Perc_Ausentes = NA))
  }
  
  # Contagem segura de ausentes
  ausentes <- sum(
    sapply(df_limpo, function(x) {
      sum(is.na(x) | (is.numeric(x) & (is.nan(x) | is.infinite(x))))
    })
  )
  
  tibble(
    Linhas = nrow(df),
    Colunas = ncol(df),
    Ausentes = ausentes,
    Perc_Ausentes = round(100 * ausentes / total_valores, 2)
  )
}

# Loop por técnica e base
resultado_ausentes <- map_dfr(names(listas), function(metodo) {
  lista <- listas[[metodo]]
  if (is.null(lista)) return(NULL)
  
  map_dfr(names(lista), function(nome_base) {
    df <- lista[[nome_base]]
    resumo <- contar_ausentes(df)
    resumo$Base <- nome_base
    resumo$Metodo <- metodo
    resumo
  })
})

# Organiza resultado
resultado_ausentes <- resultado_ausentes %>%
  select(Metodo, Base, Linhas, Colunas, Ausentes, Perc_Ausentes) %>%
  arrange(desc(Perc_Ausentes))

print(resultado_ausentes, n = Inf)
