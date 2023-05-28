library(tidyverse)
library(ggplot2)


df <- read.csv2("C:\\Users\\domin\\Desktop\\Trabalho\\bcdata.sgs.24363.csv", header=TRUE, stringsAsFactors=FALSE)
df <- df %>% mutate(data = parse_date_time(data, orders = c("dmy")))
print(df)
df$ano <- format(df$data, "%Y")
df$mes <- format(df$data, "%m")

df <- subset(df,ano >= 2009 & ano <= 2011)


df_wide <- df %>% 
  group_by(ano, mes) %>% 
  summarize(valor_total = sum(valor), .groups = 'drop') %>% 
  spread(ano, valor_total) %>% 
  gather(key = "ano", value = "valor_total", -mes)

ggplot(df_wide, aes(x = ano, y = valor_total, fill = mes)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Mês") + ylab("Valor Total") +
  ggtitle("Valor Total por Mês, Lado-a-lado por ano")


