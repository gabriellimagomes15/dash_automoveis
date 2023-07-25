
setwd("D:/Documents/dashVeiculos")

#install.packages('plotly')
library(dplyr)
library(ggplot2)
library(plotly)

options(scipen = 9999)

dados <- read.csv("dados_shiny_2022081100.csv")
dados$DATA_COLETA_METADADOS <- as.Date(dados$DATA_COLETA_METADADOS)

modelo = 'gm - chevrolet classic life/ls 1.0 vhc flexp. 4p'
dados_select <- dados %>% filter(MODELO %in% c(modelo) &
                                   UF %in% c('al','ma'))

dados_select <- dados_select %>% filter(VALOR < 2000000)
max(dados_select$VALOR)

## ALTERAR FUNDO DOS GRÁFICOS ####
theme_set(theme_bw())

## MEDIANA-DATA (GRÁFICO DE LINHA) ####
mediana_data <- dados_select %>% 
  group_by(DATA_COLETA_METADADOS,UF) %>%
  summarise(medianaValor = median(VALOR))

ggplotly(mediana_data %>% 
           ggplot() +
           geom_line(aes(x = DATA_COLETA_METADADOS, y = medianaValor, 
                         color = UF,group=UF),
                     size=1) +
           ggtitle("Média de Valor")+
           scale_color_brewer(palette="Dark2") ## ALTERANDO AS CORES
         
)

## VARIAÇÃO PREÇOS POR UF(GRÁFICO DE BOXPLOT) com e sem outlier ####
ggplotly(dados_select %>%
           ggplot()+
           geom_boxplot(aes(x = UF, y = VALOR,fill = UF) ) +
           theme(legend.position="none")+
           ggtitle("Variação do Preço por UF")+
           scale_fill_brewer(palette="Dark2") ## ALTERANDO AS CORES
         
)
 

## VARIAÇÃO KM, VALOR, PREÇO(GRÁFICO DE SCATTER) ####
ggplotly( dados_select %>% 
            ggplot()+
            geom_point(aes(x = QUILOMETRAGEM, y = VALOR,color = UF) )+
            ggtitle("Distribuição do VALOR e KM por UF")+
            scale_color_brewer(palette="Dark2") ## ALTERANDO AS CORES
)

## VARIAÇÃO PREÇOS POR UF E TIPO ANUNCIO(GRÁFICO DE BOXPLOT) com e sem outlier ####
ggplotly(dados_select %>% 
           ggplot(aes(x = TIPO_ANUNCIO, y = VALOR,fill = UF))+
           geom_boxplot( ) +
           ggtitle("Variação do Preço por Tipo de Anúncio")+
           scale_fill_brewer(palette="Dark2") ## ALTERANDO AS CORES
         
) 

" ACONTECER UM PROBLEMA QUANDO UTILIZA O GGPLOTLY NESSA SITUAÇÃO, 
ELE RESETA AS DIVISÕES E UNE TUDO EM UMA SÓ. 
UMA DAS SOLUÇÕES PARA ESSE PROBLEMA É UTILIZAR A FUNÇÃO layout() PARA
PARA CONTINUAR A DIVISÃO "
# https://plotly.com/r/reference/layout/

## CORRIGINDO PROBLEMA DO BOXPLOT COM GGPLOTLY
ggplotly(dados_select %>% 
           ggplot(aes(x = TIPO_ANUNCIO, y = VALOR,fill = UF))+
           geom_boxplot( ) +
           ggtitle("Variação do Preço por Tipo de Anúncio")+
           scale_fill_brewer(palette="Dark2") ## ALTERANDO AS CORES
) %>% layout(boxmode='group')


## frequencia por cambio(GRÁFICO DE PIZZA) ####
freq_cambio <- dados_select %>% 
  group_by(CÂMBIO) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )


"ACONTECE UM PROBLEMA QUANDO UTILIZA O GGPLOTLY NESSA SITUAÇÃO, 
ELE RESETA AS CONFIGURAÇÕES FEITAS PARA CONVERTER BARRAS EM PIE
NESSE CASO O MAIS PRÁTICO É FAZER UM GRÁFICO DE PIZZA DIRETAMENTE COM
PLOTLY"

plot_ly(freq_cambio, labels = ~CÂMBIO, values = ~prop  , type = 'pie',
        textinfo = 'label+percent',showlegend = FALSE) %>%
  layout(title = 'Quantidade por Câmbio')


## frequencia por DIREÇÃO(GRÁFICO DE PIZZA) ####
freq_direcao <- dados_select %>% 
  group_by(DIREÇÃO) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
 
plot_ly(freq_direcao, labels = ~DIREÇÃO, values = ~prop  , type = 'pie',
        textinfo = 'label+percent',showlegend = FALSE) %>%
  layout(title = 'Quantidade por Direção')




## frequencia por cor(GRÁFICO DE barras) ####
#grafico_barras_cor <- 
ggplotly(
  dados_select %>%
    group_by(COR) %>%
    summarise(QTD = n()) %>%
    ggplot() +
    geom_bar(aes(x = reorder(COR,QTD), y = QTD, fill = QTD,
                 text = paste("Cor:", COR,"\n",
                              "QTD:",QTD)),
             stat = 'identity')+
    xlab('COR') + ggtitle('Quantidade por Cor')+
    theme(legend.position = 'none'),
    tooltip = c('text')
)








