if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)

# Criar um dataframe com os resultados
resultados <- data.frame(
  Modelo = rep(c("Random Forest", "LightGBM", "SVM"), each = 4),
  Balanceamento = rep(c("Sem Balanceamento", "ROSE", "SMOTE", "Oversampling"), times = 3),
  Acuracia = c(0.9027, 0.9089, 0.9451, 0.9407, 
               0.8981, 0.9127, 0.9421, 0.9381, 
               0.8851, 0.7685, 0.8337, 0.8265),
  Balanced_Accuracy = c(0.7774, 0.9088, 0.9450, 0.9407,
                        0.7626, 0.9126, 0.9421, 0.9381,
                        0.6856, 0.7677, 0.8350, 0.8265)
)

# Criar um gráfico comparando a Acurácia
ggplot(resultados, aes(x = Balanceamento, y = Acuracia, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparação da Acurácia entre Modelos e Métodos de Balanceamento") +
  xlab("Técnica de Balanceamento") +
  ylab("Acurácia") +
  theme_minimal()

# Criar um gráfico comparando a Balanced Accuracy
ggplot(resultados, aes(x = Balanceamento, y = Balanced_Accuracy, color = Modelo, group = Modelo)) +
  geom_point(size = 4) + 
  geom_line(size = 1) +
  ggtitle("Comparação da Balanced Accuracy entre Modelos e Métodos de Balanceamento") +
  xlab("Técnica de Balanceamento") +
  ylab("Balanced Accuracy") +
  theme_minimal()


resultados <- data.frame(
  Modelo = rep(c("Random Forest", "LightGBM", "SVM"), each = 4),
  Balanceamento = rep(c("Sem Balanceamento", "ROSE", "SMOTE", "Oversampling"), times = 3),
  Sensibilidade = c(0.6517, 0.8922, 0.9247, 0.9208, 
                    0.6404, 0.8954, 0.9202, 0.9157, 
                    0.5011, 0.7633, 0.8124, 0.8059),
  Especificidade = c(0.9031, 0.9254, 0.9653, 0.9605,
                     0.8848, 0.9298, 0.9600, 0.9547,
                     0.8701, 0.7741, 0.8553, 0.8487)
)

# Criar um gráfico de dispersão Sensibilidade x Especificidade
ggplot(resultados, aes(x = Especificidade, y = Sensibilidade, color = Modelo, shape = Balanceamento)) +
  geom_point(size = 5) +
  geom_line(aes(group = Modelo), linetype = "dashed") +
  ggtitle("Comparação de Sensibilidade e Especificidade") +
  xlab("Especificidade") +
  ylab("Sensibilidade") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Reformular os dados para gráfico correto
library(reshape2)
resultados_long <- melt(resultados, id.vars = c("Modelo", "Balanceamento"), 
                        variable.name = "Métrica", value.name = "Valor")

# Criar o gráfico de evolução com separação de linhas na legenda
ggplot(resultados_long, aes(x = Balanceamento, y = Valor, group = interaction(Modelo, Métrica))) +
  geom_line(aes(color = Modelo, linetype = Métrica), size = 1.2) +
  geom_point(aes(color = Modelo, shape = Métrica), size = 4) +
  scale_linetype_manual(values = c("Sensibilidade" = "solid", "Especificidade" = "dashed")) + 
  scale_shape_manual(values = c("Sensibilidade" = 16, "Especificidade" = 17)) +
  ggtitle("Evolução da Sensibilidade e Especificidade") +
  ylab("Valor (%)") +
  xlab("Técnica de Balanceamento") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Modelo", linetype = "Métrica", shape = "Métrica")
