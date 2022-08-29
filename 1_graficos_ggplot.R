
setwd("D:/Documents/dashVeiculos")

#install.packages("dplyr")
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)

options(scipen = 9999)

t <- as.data.frame(table(dados$MODELO,dados$UF))
#sort(unique(dados$MODELO))

dados <- read.csv("dados_shiny_2022081100.csv")
dados$DATA_COLETA_METADADOS <- as.Date(dados$DATA_COLETA_METADADOS)

#modelo = 'gm - chevrolet classic life/ls 1.0 vhc flexp. 4p'
modelo = 'corolla xei 16v'
uf = c('mt','se')

dados_select <- dados %>% filter(MODELO %in% c(modelo) &
                                   UF %in% uf)

## MEDIANA-DATA (GRÁFICO DE LINHA) ####
mediana_data <- dados_select %>% 
                group_by(DATA_COLETA_METADADOS,UF) %>%
                summarise(medianaValor = median(VALOR))

mediana_data %>% 
          ggplot() +
          geom_line(aes(x = DATA_COLETA_METADADOS, y = medianaValor, 
                        color = UF,group=UF),
                    size=1) +
            ggtitle("Média de Valor")

## VARIAÇÃO PREÇOS POR UF(GRÁFICO DE BOXPLOT) com e sem outlier ####
dados_select %>%
          ggplot()+
          geom_boxplot(aes(x = UF, y = VALOR,fill = UF) ) +
          theme(legend.position="none")+
          ggtitle("Variação do Preço por UF")

## VARIAÇÃO KM, VALOR, PREÇO(GRÁFICO DE SCATTER) ####
dados_select %>% 
  ggplot()+
  geom_point(aes(x = QUILOMETRAGEM, y = VALOR,color = UF) )+
  ggtitle("Distribuição do VALOR e KM por UF")

## VARIAÇÃO PREÇOS POR UF E TIPO ANUNCIO(GRÁFICO DE BOXPLOT) com e sem outlier ####
dados_select %>% 
  ggplot(aes(x = TIPO_ANUNCIO, y = VALOR,fill = UF))+
  geom_boxplot( ) +
  theme(legend.position="none")+
  ggtitle("Variação do Preço por Tipo de Anúncio")


## frequencia por cambio(GRÁFICO DE PIZZA) ####
freq_cambio <- dados_select %>% 
                group_by(CÂMBIO) %>%
                summarise(qtd = n()) %>%
                mutate(prop = qtd / sum(qtd) *100) %>%
              mutate(ypos = cumsum(prop)- 0.5*prop )
            
freq_cambio %>%          
ggplot(aes(x="", y=prop, fill=CÂMBIO)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = paste(CÂMBIO, '\n',round(prop,2),'%') ), 
            color = "white", size=6)+ 
  ggtitle("Quantidade por Câmbio")


## frequencia por DIREÇÃO(GRÁFICO DE PIZZA) ####
freq_direcao <- dados_select %>% 
  group_by(DIREÇÃO) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

freq_direcao %>%
ggplot(aes(x="", y=prop, fill=DIREÇÃO)) +
    geom_bar(stat="identity",width=10, size = 1, color = "white") +
    coord_polar("y", start=0) +
    theme_void() + 
    geom_text(aes(y = ypos, label = paste0(round(prop,1),'%') ), 
              color = "white", size=5)+
    ggtitle("Quantidade por Direção")


## frequencia por cor(GRÁFICO DE barras) ####
dados_select %>%
  group_by(COR) %>%
  summarise(QTD = n() ) %>%
  ggplot() +
  geom_bar(aes(x = reorder(COR,QTD ),y = QTD),stat = 'identity' )+
  ggtitle("Quantidade por Cor") + xlab('COR')




