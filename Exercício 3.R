library(corrplot)
library(tidyverse)
library(ggplot2)

df <- read.csv2("C:\\Users\\domin\\Desktop\\Trabalho\\bcdata.sgs.24363.csv", header=TRUE, stringsAsFactors=FALSE)

df <- df %>% mutate(data = parse_date_time(data, orders = c("dmy")))
df$ano <- format(df$data, "%Y")
df$mes <- format(df$data, "%m")
df_2009 <- subset(df,ano == 2009)
df_2011 <- subset(df,ano == 2011)


# Análise exploratória de 2009

summary(df_2009)

# Quartis de 2009

q1_2009 <- quantile(df_2009$valor, 0.25)
q2_2009 <- quantile(df_2009$valor, 0.5)
q3_2009 <- quantile(df_2009$valor, 0.75)

# Detecção de outliers

iqr_2009 <- IQR(df_2009$valor)
lim_minimo <- q1_2009 - 1.5 * iqr_2009
lim_maximo <- q3_2009 + 1.5 * iqr_2009
outliers <- df_2009$valor[df_2009$valor < lim_minimo | df_2009$valor > lim_maximo]
print(outliers)

boxplot(df_2009$valor, 
        main = "Boxplot IBC-Br x Mês - 2009",
        ylab = "IBC-Br",
        col = "white",
        border = "cornflowerblue",
        outpch=21,
        outbg="red")


#Análise exploratória de 2011

summary(df_2011)

# Quartis de 2011

q1_2011 <- quantile(df_2011$valor, 0.25)
q2_2011 <- quantile(df_2011$valor, 0.5)
q3_2011 <- quantile(df_2011$valor, 0.75)

# Detecção de outliers

iqr_2011 <- IQR(df_2011$valor)
lim_minimo <- q1_2011 - 1.5 * iqr_2011
lim_maximo <- q3_2011 + 1.5 * iqr_2011
outliers_2011 <- df_2011$valor[df_2011$valor < lim_minimo | df_2011$valor > lim_maximo]

boxplot(df_2011$valor, 
        main = "Boxplot IBC-Br x Mês  - 2011",
        ylab = "IBC-Br",
        col = "white",
        border = "cornflowerblue",
        outpch=21,
        outbg="red")
