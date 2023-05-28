library(tidyverse)
library(ggplot2)


df <- read.csv2("C:\\Users\\domin\\Desktop\\Trabalho\\bcdata.sgs.24363.csv", header=TRUE, stringsAsFactors=FALSE)
head(df, n = 20)

df <- df %>% mutate(data = parse_date_time(data, orders = c("dmy")))
df$ano <- format(df$data, "%Y")
df$mes <- format(df$data, "%m")

#Exercício 1

#cria uma cópia do dataset original
df_subset01 <- subset(df, ano >= 2009 & ano <= 2011)
#Transforma a coluna data em um valor de data
df_subset01 <- df_subset01 %>% mutate(data = ymd(data))
#Cria a coluna de ano
df_subset01 <- df_subset01 %>% mutate(ano = format(data, "%Y"))
#Transforma a coluna em um valor numérico para extrair os dados do subset de 2009 à 2011
df_subset01$ano <- as.numeric(df_subset01$ano)

#Cria o plot
ggplot(data = df_subset01, aes(x = ano, y = valor)) +
  geom_bar(stat = "identity",
           fill = "cornflowerblue")
