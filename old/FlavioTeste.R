library(ggplot2)
library(dplyr)

data <- read.csv("dataset/online_shoppers_intention.csv")

head(data)

summary(data$SpecialDay)
table(data$SpecialDay)

conversion_rates <- data %>%
  group_by(SpecialDay) %>%
  summarise(ConversionRate= mean(Revenue))

print(conversion_rates)
    
ggplot(conversion_rates, aes(x = SpecialDay, y = ConversionRate))+
  geom_bar(stat = "identity", fill = "skyblue", color =  "black") +
  labs(
    title = "Taxa de Conversão por Proximidade de Datas Especiais",
    x = "SpecialDay",
    y = "Proporção de compras" 
  ) +
theme_minimal()