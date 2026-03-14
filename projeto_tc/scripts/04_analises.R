# ==========================================
# Script: 04_analises.R
# Objetivo: Consolidar resultados e gerar análises comparativas
# Entrada: resultados_[raw, smote, rose, sda].RData
# Saída: tabelas e gráficos salvos em resultados/
# ==========================================

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(grid)

dir_processed <- "data/processed/"
dir_metricas  <- "resultados/metricas/"
dir_graficos  <- "resultados/graficos/"

# Criar pastas de resultados
if (!dir.exists(dir_metricas)) dir.create(dir_metricas, recursive = TRUE)
if (!dir.exists(dir_graficos)) dir.create(dir_graficos, recursive = TRUE)


# --------------------------------------------
# Carrega resultados (*.rds)
# --------------------------------------------
arquivos <- list.files(dir_processed, pattern = "resultados_.*\\.rds$", full.names = TRUE)

if (length(arquivos) == 0) {
  stop("Nenhum arquivo de resultados encontrado em data/processed/.")
}

extrair_metodo <- function(caminho) {
  gsub("resultados_|\\.rds", "", basename(caminho))
}

lista_resultados <- list()

for (arq in arquivos) {
  nome <- extrair_metodo(arq)
  
  objeto <- readRDS(arq)  # <-- segurança total
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
# Análise detalhada por base
# ------------------------------------------------------------
metricas_por_base <- resultados_finais %>%
  select(Base, Balanceamento, Modelo, Acuracia, Precisao, Recall, F1)

write_csv(metricas_por_base, file.path(dir_metricas, "metricas_por_base.csv"))

# ------------------------------------------------------------
# Novos gráficos solicitados
# ------------------------------------------------------------

# ------------------------------------------------------------
# Boxplot de Acurácia com Q1 e Q3
# ------------------------------------------------------------

# Calcular quartis
resumo_acc <- metricas_por_base %>%
  group_by(Balanceamento) %>%
  summarise(
    Q1 = quantile(Acuracia, 0.25, na.rm = TRUE),
    Q3 = quantile(Acuracia, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

grafico_box_acc <- ggplot(metricas_por_base,
                          aes(x = Balanceamento,
                              y = Acuracia,
                              fill = Balanceamento)) +
  
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  
  # Q1
  geom_text(data = resumo_acc,
            aes(x = Balanceamento,
                y = Q1,
                label = round(Q1, 3)),
            vjust = 1.3,
            size = 3.5,
            inherit.aes = FALSE) +
  
  # Q3
  geom_text(data = resumo_acc,
            aes(x = Balanceamento,
                y = Q3,
                label = round(Q3, 3)),
            vjust = -0.6,
            size = 3.5,
            inherit.aes = FALSE) +
  
  labs(
    title = "Boxplot de Acurácia por Técnica de Balanceamento",
    x = "Técnica de Balanceamento",
    y = "Acurácia"
  ) +
  ylim(0, 1.05) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(dir_graficos, "boxplot_Acuracia_por_base.png"),
       grafico_box_acc,
       width = 7,
       height = 5)

# ------------------------------------------------------------
# Boxplot de F1 com Q1, Mediana e Q3 dentro da caixa
# ------------------------------------------------------------

# Quartis
resumo_f1 <- metricas_por_base %>%
  group_by(Balanceamento) %>%
  summarise(
    Q1 = quantile(F1, 0.25, na.rm = TRUE),
    Mediana = median(F1, na.rm = TRUE),
    Q3 = quantile(F1, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Média e desvio padrão
resumo_sd <- metricas_por_base %>%
  group_by(Balanceamento) %>%
  summarise(
    media = mean(F1, na.rm = TRUE),
    sd = sd(F1, na.rm = TRUE),
    .groups = "drop"
  )

grafico_box_f1 <- ggplot(metricas_por_base,
                         aes(x = Balanceamento,
                             y = F1,
                             fill = Balanceamento)) +
  
  # BARRA DE ERRO (camada do fundo)
  geom_errorbar(
    data = resumo_sd,
    aes(
      x = Balanceamento,
      ymin = pmax(media - sd, 0),
      ymax = pmin(media + sd, 1)
    ),
    width = 0.15,
    color = "black",
    size = 0.7,
    inherit.aes = FALSE
  ) +
  
  # BOXPLOT (por cima)
  geom_boxplot(
    width = 0.6,
    coef = 0,
    outlier.shape = NA
  ) +
  
  # Q1
  geom_text(data = resumo_f1,
            aes(x = Balanceamento,
                y = Q1 + 0.02,
                label = round(Q1, 3)),
            size = 3.2,
            fontface = "bold",
            inherit.aes = FALSE) +
  
  # Mediana
  geom_text(data = resumo_f1,
            aes(x = Balanceamento,
                y = Mediana + 0.02,
                label = round(Mediana, 3)),
            size = 3.5,
            fontface = "bold",
            inherit.aes = FALSE) +
  
  # Q3
  geom_text(data = resumo_f1,
            aes(x = Balanceamento,
                y = Q3 - 0.015,
                label = round(Q3, 3)),
            size = 3.2,
            fontface = "bold",
            inherit.aes = FALSE) +
  
  labs(
    title = "Boxplot de F1-Score por Técnica de Balanceamento",
    x = "Técnica de Balanceamento",
    y = "F1-Score"
  ) +
  ylim(0, 1.05) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

ggsave(file.path(dir_graficos, "boxplot_F1_por_base.png"),
       grafico_box_f1,
       width = 7,
       height = 5)


# ------------------------------------------------------------
# Barras agrupadas por técnica (média real por técnica)
# com barra de erro (desvio padrão)
# ------------------------------------------------------------

# 1️⃣ Calcular média e desvio padrão por técnica
metricas_tecnica <- resultados_finais %>%
  group_by(Balanceamento) %>%
  summarise(
    Media_Acuracia = mean(Acuracia, na.rm = TRUE),
    SD_Acuracia    = sd(Acuracia, na.rm = TRUE),
    Media_F1       = mean(F1, na.rm = TRUE),
    SD_F1          = sd(F1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -Balanceamento,
    names_to = c(".value", "Metrica"),
    names_pattern = "(Media|SD)_(.*)"
  )

# 2️⃣ Gráfico com barra de erro
grafico_barras_metricas <- ggplot(metricas_tecnica,
                                  aes(x = Balanceamento,
                                      y = Media,
                                      fill = Metrica)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  
  geom_errorbar(aes(ymin = Media - SD,
                    ymax = Media + SD),
                position = position_dodge(width = 0.8),
                width = 0.2,
                size = 0.6) +
  
  geom_text(aes(y = (Media - SD) / 2,label = round(Media, 3)),
            position = position_dodge(width = 0.8),
            vjust = -0.4,
            size = 4) +
  
  labs(
    title = "Comparação de Acurácia e F1-Score por Técnica de Balanceamento\n(com Desvio Padrão entre Bases)",
    x = "Técnica de Balanceamento",
    y = "Valor Médio"
  ) +
  
  coord_cartesian(ylim = c(0, 1.05)) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# 3️⃣ Salvar gráfico
ggsave(file.path(dir_graficos, "barras_Acuracia_F1_com_desvio.png"),
       grafico_barras_metricas,
       width = 8,
       height = 5)

# ------------------------------------------------------------
# Gráficos comparativos (originais)
# ------------------------------------------------------------

tabela_f1_modelo_tecnica <- resultados_finais %>%
  group_by(Balanceamento, Modelo) %>%
  summarise(
    Media = mean(F1, na.rm = TRUE),
    Desvio = sd(F1, na.rm = TRUE),
    .groups = "drop"
  )

grafico_f1_modelo_tecnica <- ggplot(tabela_f1_modelo_tecnica,
                                    aes(x = Modelo,
                                        y = Media,
                                        fill = Modelo)) +
  
  geom_bar(stat = "identity",
           width = 0.7) +
  
  geom_errorbar(aes(ymin = Media - Desvio,
                    ymax = Media + Desvio),
                width = 0.2,
                size = 0.6) +
  
  facet_wrap(~ Balanceamento, ncol = 3, scales = "free_x") +

  labs(
    title = "F1-Score Médio por Modelo e Técnica de Balanceamento\n(com Desvio Padrão entre Bases)",
    x = "Modelo",
    y = "F1-Score Médio"
  ) +
  
  coord_cartesian(ylim = c(0, 1.05)) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.5, "lines")
  )

ggsave(
  file.path(dir_graficos, "f1_modelo_tecnica_barplot.png"),
  grafico_f1_modelo_tecnica,
  width = 12,
  height = 8
)

# ------------------------------------------------------------
# GRÁFICO: Acurácia por Modelo × Balanceamento
# (com desvio padrão entre bases)
# ------------------------------------------------------------


acuracia_modelo_bal <- resultados_finais %>%
  group_by(Modelo, Balanceamento) %>%
  summarise(
    Media = mean(Acuracia, na.rm = TRUE),
    Desvio = sd(Acuracia, na.rm = TRUE),
    .groups = "drop"
  )

# Garantir ordem consistente do balanceamento
acuracia_modelo_bal$Balanceamento <- factor(
  acuracia_modelo_bal$Balanceamento,
  levels = c("RAW", "OVERSAMPLING", "UNDERSAMPLING", "SMOTE", "ROSE")
)


grafico_acuracia_modelo_bal <- ggplot(acuracia_modelo_bal,
                                      aes(x = Modelo,
                                          y = Media,
                                          fill = Balanceamento)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  
  geom_errorbar(aes(ymin = Media - Desvio,
                    ymax = Media + Desvio),
                position = position_dodge(width = 0.8),
                width = 0.2,
                size = 0.6) +
  
  labs(
    title = "Acurácia Média por Modelo e Técnica de Balanceamento\n(com Desvio Padrão entre Bases)",
    x = "Modelo",
    y = "Acurácia Média",
    fill = "Balanceamento"
  ) +
  
  coord_cartesian(ylim = c(0, 1.05)) +   # evita cortar barra
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5)
  )


# Salvar gráfico


ggsave(
  file.path(dir_graficos, "acuracia_modelo_balanceamento_com_desvio.png"),
  grafico_acuracia_modelo_bal,
  width = 11,
  height = 6
)

# ------------------------------------------------------------
# TABELA GRÁFICA: Acurácia Média por Base e Modelo
# ------------------------------------------------------------

# Calcular média da acurácia por Base e Modelo
tabela_base_modelo <- metricas_por_base %>%
  group_by(Base, Modelo) %>%
  summarise(
    Acuracia = mean(Acuracia, na.rm = TRUE),
    .groups = "drop"
  )

# Salvar versão CSV
write_csv(
  tabela_base_modelo,
  file.path(dir_metricas, "tabela_media_acuracia_base_modelo.csv")
)

# ------------------------------------------------------------
# Gerar heatmap (tabela em forma de gráfico)
# ------------------------------------------------------------

grafico_tabela_acuracia <- ggplot(tabela_base_modelo,
                                  aes(x = Modelo,
                                      y = Base,
                                      fill = Acuracia)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Acuracia, 3)),
            size = 4,
            color = "black") +
  scale_fill_gradient(low = "#f7f7f7", high = "#1b7837") +
  labs(
    title = "Acurácia Média por Base de Dados e Modelo",
    x = "Modelo",
    y = "Base de Dados",
    fill = "Acurácia"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  file.path(dir_graficos, "tabela_grafica_acuracia_base_modelo.png"),
  grafico_tabela_acuracia,
  width = 9,
  height = 6
)

# ------------------------------------------------------------
# Acurácia média global por modelo (todas as técnicas)
# ------------------------------------------------------------

acuracia_media_modelo <- resultados_finais %>%
  group_by(Modelo) %>%
  summarise(
    Media = mean(Acuracia, na.rm = TRUE),
    Desvio = sd(Acuracia, na.rm = TRUE),
    .groups = "drop"
  )

grafico_acuracia_media_modelo <- ggplot(acuracia_media_modelo,
                                        aes(x = Modelo,
                                            y = Media,
                                            fill = Modelo)) +
  geom_bar(stat = "identity", width = 0.6) +
  
  geom_errorbar(aes(ymin = Media - Desvio,
                    ymax = Media + Desvio),
                width = 0.2,
                size = 0.8) +
  
  # Média dentro da barra
  geom_text(aes(y = (Media/2) + 0.05,
                label = round(Media, 3)),
            vjust = 1.5,
            size = 4) +
  
  labs(
    title = "Acurácia Média por Modelo\n(com Desvio Padrão entre Bases)",
    x = "Modelo",
    y = "Acurácia Média"
  ) +
  ylim(0, 1.05) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  file.path(dir_graficos, "acuracia_media_por_modelo_com_desvio.png"),
  grafico_acuracia_media_modelo,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Gráfico Acurácia média e desvio padrão por Base e Modelo
# ------------------------------------------------------------

acuracia_base_modelo <- resultados_finais %>%
  group_by(Base, Modelo) %>%
  summarise(
    Media = mean(Acuracia, na.rm = TRUE),
    Desvio = sd(Acuracia, na.rm = TRUE),
    .groups = "drop"
  )

grafico_acuracia_base_modelo <- ggplot(acuracia_base_modelo,
                                       aes(x = Modelo,
                                           y = Media,
                                           fill = Modelo)) +
  
  geom_bar(stat = "identity", width = 0.6) +
  
  geom_errorbar(aes(ymin = Media - Desvio,
                    ymax = Media + Desvio),
                width = 0.2,
                size = 0.6) +
  
  facet_wrap(~ Base, ncol = 3, scales = "free_x") +
  
  labs(
    title = "Acurácia Média por Base de Dados e Modelo\n(com Desvio Padrão entre Técnicas)",
    x = "Modelo",
    y = "Acurácia Média"
  ) +
  
  ylim(0, 1.05) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

# Salvar gráfico
ggsave(
  file.path(dir_graficos, "acuracia_media_por_base_modelo_com_desvio.png"),
  grafico_acuracia_base_modelo,
  width = 12,
  height = 8
)



# PRECISION E RECALL
# ------------------------------------------------------------
# 1️⃣ Barplot: Precisão e Recall médio por Modelo (com desvio padrão)
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

metricas_media_modelo <- resultados_finais %>%
  group_by(Modelo) %>%
  summarise(
    Media_Precisao = mean(Precisao, na.rm = TRUE),
    SD_Precisao    = sd(Precisao, na.rm = TRUE),
    Media_Recall   = mean(Recall, na.rm = TRUE),
    SD_Recall      = sd(Recall, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -Modelo,
    names_to = c(".value", "Metrica"),
    names_pattern = "(Media|SD)_(.*)"
  )

grafico_bar_modelo_pr_rc <- ggplot(metricas_media_modelo,
                                   aes(x = Modelo,
                                       y = Media,
                                       fill = Metrica)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  
  geom_errorbar(aes(ymin = Media - SD,
                    ymax = Media + SD),
                position = position_dodge(width = 0.8),
                width = 0.2,
                size = 0.6) +
  
  geom_text(aes(y = (Media - SD) / 2 ,label = round(Media, 3)),
            position = position_dodge(width = 0.8),
            vjust = -0.4,
            size = 3.8) +
  
  labs(
    title = "Precisão e Recall Médio por Modelo\n(com Desvio Padrão entre Bases)",
    x = "Modelo",
    y = "Valor Médio",
    fill = "Métrica"
  ) +
  
  coord_cartesian(ylim = c(0, 1.05)) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  file.path(dir_graficos, "bar_precisao_recall_modelo_com_desvio.png"),
  grafico_bar_modelo_pr_rc,
  width = 9,
  height = 5
)

# ------------------------------------------------------------
# 2️⃣ Gráfico de barras: Precisão por Base × Modelo
# (com barra de erro e facets por base)
# ------------------------------------------------------------


tabela_base_modelo_prec <- metricas_por_base %>%
  group_by(Base, Modelo) %>%
  summarise(
    Media = mean(Precisao, na.rm = TRUE),
    Desvio = sd(Precisao, na.rm = TRUE),
    .groups = "drop"
  )

grafico_bar_prec_base_modelo <- ggplot(tabela_base_modelo_prec,
                                       aes(x = Modelo,
                                           y = Media,
                                           fill = Modelo)) +
  
  geom_bar(stat = "identity",
           width = 0.7) +
  
  geom_errorbar(aes(ymin = Media - Desvio,
                    ymax = Media + Desvio),
                width = 0.2,
                size = 0.6) +
  
  facet_wrap(~ Base, ncol = 3, scales = "free_x") +
  
  labs(
    title = "Precisão Média por Base e Modelo\n(com Desvio Padrão entre Técnicas)",
    x = "Modelo",
    y = "Precisão Média"
  ) +
  
  coord_cartesian(ylim = c(0, 1.05)) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.5, "lines")
  )

ggsave(
  file.path(dir_graficos, "precisao_base_modelo_barplot.png"),
  grafico_bar_prec_base_modelo,
  width = 12,
  height = 8
)


# ------------------------------------------------------------
# 3️⃣ Gráfico de barras: Recall por Base × Modelo
# (com barra de erro e facets por base)
# ------------------------------------------------------------


tabela_base_modelo_rec <- metricas_por_base %>%
  group_by(Base, Modelo) %>%
  summarise(
    Media = mean(Recall, na.rm = TRUE),
    Desvio = sd(Recall, na.rm = TRUE),
    .groups = "drop"
  )

grafico_bar_rec_base_modelo <- ggplot(tabela_base_modelo_rec,
                                      aes(x = Modelo,
                                          y = Media,
                                          fill = Modelo)) +
  
  geom_bar(stat = "identity",
           width = 0.7) +
  
  geom_errorbar(
    aes(
      ymin = pmax(Media - Desvio, 0),
      ymax = pmin(Media + Desvio, 1)
    ),
    width = 0.2,
    size = 0.6
  ) +
  facet_wrap(~ Base, ncol = 3, scales = "free_x") +
  
  labs(
    title = "Recall Médio por Base e Modelo\n(com Desvio Padrão entre Técnicas)",
    x = "Modelo",
    y = "Recall Médio"
  ) +
  
  coord_cartesian(ylim = c(0, 1.05)) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.5, "lines")
  )

ggsave(
  file.path(dir_graficos, "recall_base_modelo_barplot.png"),
  grafico_bar_rec_base_modelo,
  width = 12,
  height = 8
)
# ------------------------------------------------------------
# 3️⃣ Boxplot: Precisão e Recall por Técnica
#    Mostrando Q1, Mediana e Q3 (extremos da caixa colorida)
# ------------------------------------------------------------


metricas_long_bal <- metricas_por_base %>%
  pivot_longer(cols = c(Precisao, Recall),
               names_to = "Metrica",
               values_to = "Valor")

# Calcular Q1, Mediana e Q3 por Técnica e Métrica
resumo_quartis <- metricas_long_bal %>%
  group_by(Balanceamento, Metrica) %>%
  summarise(
    Q1 = quantile(Valor, 0.25, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE),
    Q3 = quantile(Valor, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

grafico_box_bal <- ggplot(metricas_long_bal,
                          aes(x = Balanceamento,
                              y = Valor,
                              fill = Metrica)) +
  
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.shape = NA,
               width = 0.7) +
  
  geom_text(data = resumo_quartis,
            aes(x = Balanceamento,
                y = Q1,
                label = round(Q1, 3),
                group = Metrica),
            position = position_dodge(width = 0.8),
            vjust = 1.3,
            size = 3.2,
            inherit.aes = FALSE) +
  
  # geom_text(data = resumo_quartis,
  #           aes(x = Balanceamento,
  #               y = Mediana,
  #               label = round(Mediana, 3),
  #               group = Metrica),
  #           position = position_dodge(width = 0.8),
  #           vjust = -0.3,
  #           size = 3.2,
  #           fontface = "bold",
  #           inherit.aes = FALSE) +
  
  geom_text(data = resumo_quartis,
            aes(x = Balanceamento,
                y = Q3,
                label = round(Q3, 3),
                group = Metrica),
            position = position_dodge(width = 0.8),
            vjust = -0.8,
            size = 3.2,
            inherit.aes = FALSE) +
  
  labs(
    title = "Distribuição de Precisão e Recall por Técnica de Balanceamento",
    x = "Técnica de Balanceamento",
    y = "Valor",
    fill = "Métrica"
  ) +
  ylim(0, 1.05) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  file.path(dir_graficos, "boxplot_precisao_recall_balanceamento.png"),
  grafico_box_bal,
  width = 9,
  height = 5
)

# ------------------------------------------------------------
# Mensagem final
# ------------------------------------------------------------
cat("\nAnálises concluídas com sucesso!")
cat("\nArquivos salvos em:")
cat("\n - resultados/metricas/metricas_resumo.csv")
cat("\n - resultados/metricas/metricas_por_base.csv")
cat("\n - resultados/graficos/*.png\n")
