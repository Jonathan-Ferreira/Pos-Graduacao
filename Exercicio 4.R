library(dplyr)
library(tidyverse)

df <- read.csv2("C:\\Users\\domin\\Desktop\\Trabalho\\bcdata.sgs.24363.csv", header=TRUE, stringsAsFactors=FALSE)


df <- df %>% mutate(data = parse_date_time(data, orders = c("dmy")))
df$ano <- format(df$data, "%Y")
df$mes <- format(df$data, "%m")
df_sub <- subset(df,ano == 2010)
df_sub$mes <- as.numeric(as.character(df_sub$mes))


modelo_regressao <- lm(valor ~ mes, data = df_sub)

summary(modelo_regressao)

modelo_regressao_poly <- lm(valor ~ poly(mes, degree = 1), data = df_sub)

summary(modelo_regressao_poly)
