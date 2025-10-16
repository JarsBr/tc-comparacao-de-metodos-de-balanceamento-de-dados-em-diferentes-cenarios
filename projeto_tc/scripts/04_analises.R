# ==========================================
# Script: 04_analises.R
# Objetivo: Consolidar resultados e gerar análises comparativas
# Entrada: resultados_[raw, smote, rose, sda].RData
# Saída: tabelas e gráficos salvos em resultados/
# ==========================================

rm(list = ls())

# Pacotes necessários
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# Caminhos
dir_processed <- "data/processed/"
dir_metricas  <- "resultados/metricas/"
dir_graficos  <- "resultados/graficos/"

# Cria pastas de saída, se não existirem
if (!dir.exists(dir_metricas)) dir.create(dir_metricas, recursive = TRUE)
if (!dir.exists(dir_graficos)) dir.create(dir_graficos, recursive = TRUE)

# ------------------------------------------------------------
# Carrega resultados
# ------------------------------------------------------------
arquivos <- list.files(dir_processed, pattern = "resultados_.*\\.RData$", full.names = TRUE)

if (length(arquivos) == 0) {
  stop("⚠️ Nenhum arquivo de resultados encontrado em data/processed/.
       Execute antes o script 03_modelos.R.")
}

# Função auxiliar: extrair nome do método (raw, smote, etc.)
extrair_metodo <- function(caminho) {
  gsub("resultados_|\\.RData", "", basename(caminho))
}

lista_resultados <- list()

for (arq in arquivos) {
  load(arq)
  nome <- extrair_metodo(arq)
  objeto <- get(paste0("resultados_", nome))
  objeto$Balanceamento <- toupper(nome)
  lista_resultados[[nome]] <- objeto
}

# Junta tudo
resultados_finais <- bind_rows(lista_resultados)

# ------------------------------------------------------------
# Consolidação geral (média por modelo e balanceamento)
# ------------------------------------------------------------
metricas_resumo <- resultados_finais %>%
  group_by(Balanceamento, Modelo) %>%
  summarise(
    Acuracia = mean(Acuracia, na.rm = TRUE),
    Precisao = mean(Precisao, na.rm = TRUE),
    Recall   = mean(Recall, na.rm = TRUE),
    F1       = mean(F1, na.rm = TRUE)
  ) %>%
  arrange(Balanceamento, desc(F1))

# Salva em CSV
write_csv(metricas_resumo, file.path(dir_metricas, "metricas_resumo.csv"))

# ------------------------------------------------------------
# 🔍 Análise detalhada por base
# ------------------------------------------------------------
metricas_por_base <- resultados_finais %>%
  select(Base, Balanceamento, Modelo, Acuracia, Precisao, Recall, F1)

write_csv(metricas_por_base, file.path(dir_metricas, "metricas_por_base.csv"))

# ------------------------------------------------------------
# 📊 Gráficos comparativos
# ------------------------------------------------------------

# 1️⃣ Comparação geral de F1 por modelo e técnica
grafico_f1 <- ggplot(metricas_resumo, aes(x = Balanceamento, y = F1, fill = Modelo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Comparação de F1-Score entre Técnicas de Balanceamento",
    x = "Técnica de Balanceamento", y = "F1-Score"
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_graficos, "comparacao_F1.png"), grafico_f1, width = 8, height = 5)

# 2️⃣ Boxplot de acurácia por técnica
grafico_acc <- ggplot(resultados_finais, aes(x = Balanceamento, y = Acuracia, fill = Balanceamento)) +
  geom_boxplot() +
  labs(title = "Distribuição da Acurácia por Técnica de Balanceamento", y = "Acurácia", x = "") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(file.path(dir_graficos, "boxplot_Acuracia.png"), grafico_acc, width = 7, height = 5)

# 3️⃣ Heatmap comparando F1 entre modelos e técnicas
grafico_heat <- metricas_resumo %>%
  ggplot(aes(x = Modelo, y = Balanceamento, fill = F1)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(F1, 3)), color = "black", size = 4) +
  scale_fill_gradient(low = "#f0f0f0", high = "#1b7837") +
  labs(title = "Heatmap de F1-Score por Modelo e Técnica") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_graficos, "heatmap_F1.png"), grafico_heat, width = 7, height = 5)

# ------------------------------------------------------------
# Mensagem final
# ------------------------------------------------------------
cat("\n✅ Análises concluídas com sucesso!")
cat("\n💾 Arquivos salvos em:")
cat("\n   - resultados/metricas/metricas_resumo.csv")
cat("\n   - resultados/metricas/metricas_por_base.csv")
cat("\n   - resultados/graficos/*.png\n")
