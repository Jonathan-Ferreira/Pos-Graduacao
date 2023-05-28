library(dplyr)
library(tidyverse)

df <- read.csv2("C:\\Users\\domin\\Desktop\\Trabalho\\bcdata.sgs.24363.csv", header=TRUE, stringsAsFactors=FALSE)

df <- df %>% mutate(data = parse_date_time(data, orders = c("dmy")))
df$ano <- format(df$data, "%Y")
df$mes <- format(df$data, "%m")

# Ano 2010

df_2010 <- subset(df,ano == 2010)
df_2010$mes <- as.numeric(as.character(df_2010$mes))
df_2010$ano <- as.numeric(as.character(df_2010$ano))

----------------cor(df_2010$mes, df_2010$valor)
cor.test(df_2010$mes, df_2010$valor)

which(df_2010$valor > quantile(df_2010$valor, 0.25))

mod = lm(df_2010$valor~df_2010$mes)

summary (mod)

plot(df_2010$mes, 
     df_2010$valor, 
     main = "Regressão linear - 2010",
     pch = 19,
     col = "black",
     xlab = "Mês", 
     ylab = "Valor")
abline(mod, col="red")

rp_2010 <- lm(valor ~ poly(mes,2, degree = 1), data = df_2010)
ggplot(df_2010, 
       aes(y=valor, x=mes))+
       ggtitle("Regressão Polinomial - 2010") + 
       theme(plot.title = element_text(hjust = 0.5))+
       geom_point(size=2.5,pch=19,col='red',fill='brown4')+
       geom_smooth(method = 'lm',col = "red")

summary(rp_2010)


# Ano 2011

df_2011 <- subset(df,ano == 2011)
df_2011$mes <- as.numeric(as.character(df_2011$mes))
df_2011$ano <- as.numeric(as.character(df_2011$ano))

----------------cor(df_2011$mes, df_2011$valor)
cor.test(df_2011$mes, df_2011$valor)

plot(df_2011$mes, df_2011$valor)
abline(df_2011)

which(df_2011$valor > quantile(df_2011$valor, 0.25))

mod = lm(df_2011$valor~df_2011$mes)

plot(df_2011$mes, df_2011$valor,
     main = "Regressão linear- 2011",
     pch = 19,
     col = "black",
     xlab = "Mês", 
     ylab = "Valor")
abline(mod, col="red")

modelo_regressao <- lm(valor ~ mes, data = df_2011)

summary(modelo_regressao)

rp_2011 <- lm(valor ~ poly(mes,2, degree = 1), data = df_2011)
ggplot(df_2011, 
       aes(y=valor, x=mes))+
       ggtitle("Regressão Polinomial - 2011") + 
       theme(plot.title = element_text(hjust = 0.5))+
       geom_point(size=2.5,pch=19,col='red',fill='brown4')+
       geom_smooth(method = 'lm',col = "red")

summary(rp_2011)

