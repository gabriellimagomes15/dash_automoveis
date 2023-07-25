

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
#install.packages("shinydashboard")
library(shinydashboard)

#dados <- read.csv("dados_shiny_2022081822.csv")
dados <- read.csv("dados.csv")
#unique(dados$MODELO_ORIGIN)

options(warn = -1)

theme_set(theme_bw())

View(dados %>% filter(DATA_COLETA_METADADOS == max(dados$DATA_COLETA_METADADOS))
)
     