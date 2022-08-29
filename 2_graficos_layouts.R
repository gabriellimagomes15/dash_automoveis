
setwd("D:/Documents/dashVeiculos")

#install.packages("dplyr")
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)

options(scipen = 9999)

 

dados <- read.csv("dados_shiny_2022081822.csv")
dados$DATA_COLETA_METADADOS <- as.Date(dados$DATA_COLETA_METADADOS)

modelo <- 'corolla xei 16v'
uf <- c('mt','se')

dados_select <- dados %>% filter(MODELO == modelo & 
                   UF %in% uf)


## ALTERAR FUNDO DOS GRÁFICOS PARA BRANCO ####
theme_set(theme_bw())

## MEDIANA-DATA (GRÁFICO DE LINHA) ####
mediana_data <- dados_select %>% 
  group_by(DATA_COLETA_METADADOS,UF) %>%
  summarise(medianaValor = median(VALOR))

mediana_data %>% 
  ggplot() +
  geom_line(aes(x = DATA_COLETA_METADADOS, y = medianaValor, 
                color = UF,group=UF),
            size=1) +
  ggtitle("Média de Valor") +
  scale_color_brewer(palette="Dark2") ## ALTERANDO AS CORES

# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually


## VARIAçãO PREÇOS POR UF(GRÁFICO DE BOXPLOT) com e sem outlier ####
dados_select %>%
  ggplot()+
  geom_boxplot(aes(x = UF, y = VALOR,fill = UF) ) +
  theme(legend.position="none")+
  ggtitle("Variaçãoo do Preço por UF")+
  scale_fill_brewer(palette="Dark2") ## ALTERANDO AS CORES

## VARIAÇÃO KM, VALOR, PREÇO(GRÁFICO DE SCATTER) ####
dados_select %>% 
  ggplot()+
  geom_point(aes(x = QUILOMETRAGEM, y = VALOR,color = UF) )+
  ggtitle("Distribuição do VALOR e KM por UF")+
  scale_color_brewer(palette="Dark2") ## ALTERANDO AS CORES

## VARIAÇAo PREÇOS POR UF E TIPO ANUNCIO(GRÁFICO DE BOXPLOT) com e sem outlier ####
dados_select %>% 
  ggplot(aes(x = TIPO_ANUNCIO, y = VALOR,fill = UF))+
  geom_boxplot( ) +
  theme(legend.position="none")+
  ggtitle("Variação do Preço por Tipo de Anúncio")+
  scale_fill_brewer(palette="Dark2") ## ALTERANDO AS CORES


## frequencia por cambio(GR?FICO DE PIZZA) ####
freq_cambio <- dados_select %>% 
  group_by(CÂMBIO) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )


ggplot(freq_cambio, aes(x="", y=prop, fill=CÂMBIO)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = paste(CÂMBIO, '\n',round(prop,2),'%') ), 
            color = "white", size=6)+ 
  ggtitle("Quantidade por Câmbio")+
  scale_fill_brewer(palette="Dark2") ## ALTERANDO AS CORES


## frequencia por DIRE??O(GRÁFICO DE PIZZA) ####
freq_direcao <- dados_select %>% 
  group_by(DIREÇÃO) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

freq_direcao %>%
  ggplot() +
  geom_bar(aes(x="", y=prop, fill=DIREÇÃO),
           stat="identity",width=10, size = 1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  geom_text(aes(y = ypos, label = paste0(round(prop,1),'%') ), 
            color = "white", size=5)+
  ggtitle("Quantidade por DireÇÃo")+
  scale_fill_brewer(palette="Dark2") ## ALTERANDO AS CORES


## frequencia por cor(GRÁ FICO DE barras) ####
dados_select %>%
  group_by(COR) %>%
  summarise(QTD = n() ) %>%
  ggplot() +
  geom_bar(aes(x = reorder(COR,QTD ),y = QTD,fill=QTD),stat = 'identity' )+
  ggtitle("Quantidade por  Cor") + xlab('COR')+
  theme(legend.position="none")
  

  



