
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

## MEDIANA-DATA (GR?FICO DE LINHA) ####
mediana_data <- dados_select %>% 
                group_by(DATA_COLETA_METADADOS,UF) %>%
                summarise(medianaValor = median(VALOR))

mediana_data %>% 
          ggplot() +
          geom_line(aes(x = DATA_COLETA_METADADOS, y = medianaValor, 
                        color = UF,group=UF),
                    size=1) +
            ggtitle("M?dia de Valor")

## VARIA??O PRE?OS POR UF(GR?FICO DE BOXPLOT) com e sem outlier ####
dados_select %>%
          ggplot()+
          geom_boxplot(aes(x = UF, y = VALOR,fill = UF) ) +
          theme(legend.position="none")+
          ggtitle("Varia??o do Pre?o por UF")

## VARIA??O KM, VALOR, PRE?O(GR?FICO DE SCATTER) ####
dados_select %>% 
  ggplot()+
  geom_point(aes(x = QUILOMETRAGEM, y = VALOR,color = UF) )+
  ggtitle("Distribui??o do VALOR e KM por UF")

## VARIA??O PRE?OS POR UF E TIPO ANUNCIO(GR?FICO DE BOXPLOT) com e sem outlier ####
dados_select %>% 
  ggplot(aes(x = TIPO_ANUNCIO, y = VALOR,fill = UF))+
  geom_boxplot( ) +
  theme(legend.position="none")+
  ggtitle("Varia??o do Pre?o por Tipo de An?ncio")


## frequencia por cambio(GR?FICO DE PIZZA) ####
freq_cambio <- dados_select %>% 
                group_by(C?MBIO) %>%
                summarise(qtd = n()) %>%
                mutate(prop = qtd / sum(qtd) *100) %>%
              mutate(ypos = cumsum(prop)- 0.5*prop )
            
freq_cambio %>%          
ggplot(aes(x="", y=prop, fill=C?MBIO)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = paste(C?MBIO, '\n',round(prop,2),'%') ), 
            color = "white", size=6)+ 
  ggtitle("Quantidade por C?mbio")


## frequencia por DIRE??O(GR?FICO DE PIZZA) ####
freq_direcao <- dados_select %>% 
  group_by(DIRE??O) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

freq_direcao %>%
ggplot(aes(x="", y=prop, fill=DIRE??O)) +
    geom_bar(stat="identity",width=10, size = 1, color = "white") +
    coord_polar("y", start=0) +
    theme_void() + 
    geom_text(aes(y = ypos, label = paste0(round(prop,1),'%') ), 
              color = "white", size=5)+
    ggtitle("Quantidade por Dire??o")


## frequencia por cor(GR?FICO DE barras) ####
dados_select %>%
  group_by(COR) %>%
  summarise(QTD = n() ) %>%
  ggplot() +
  geom_bar(aes(x = reorder(COR,QTD ),y = QTD),stat = 'identity' )+
  ggtitle("Quantidade por Cor") + xlab('COR')




