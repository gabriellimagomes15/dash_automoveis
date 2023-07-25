
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinydashboard)
library(shinyjs)
library(RColorBrewer)
#install.packages('rjson')
library(jsonlite)
#install.packages('dipsaus')
library(dipsaus)

options(scipen = 9999)
theme_set(theme_bw())

dados <- read.csv("dados.csv")
dados$DATA_COLETA_METADADOS <- as.Date(dados$DATA_COLETA_METADADOS)

metadados_filtros <- read_json('metadados_filtros.json')



for(linha in metadados_filtros){
  print(linha)
  id_ = trimws(linha['idAno'])
  x = linha['ano']
  anos = list(x = id_)
  
}
anos

a <- '1111'
b <- 'aaa'
c(a = b)

 
novoElemento <- function(){
  r <- fluidRow(
    tags$div(id="graficos",
     fluidRow(
        column(width = 12,
               box(width = '100%',
                   column(width = 8, plotlyOutput("grafico_media_valores2") ),
                   column(width = 4, plotlyOutput("grafico_boxplot_preco2") )
               )       
        )
      ), #FIM PRIMEIRA LINHA GRAFICOS
      fluidRow(
        column(width = 12,
               box(width = '100%',
                   column(width = 5, plotlyOutput("grafico_km_valor2") ),
                   column(width = 4, plotlyOutput("grafico_tipo_anuncio2") ),
                   column(width = 3, plotlyOutput("grafico_pie_cambio2") )
               ) 
        )
      ), # FIM SEGUNDA LINHA GRÁFICOS
      fluidRow(
        column(width = 12,
               box(width = '100%',
                   column(width = 3,plotlyOutput("grafico_pie_direcao2") ),
                   column(width = 9,plotlyOutput("grafico_bar_cor2") )
               ) 
        )
      )# FIM TERCEIRA LINHA GRÁFICOS
    ))
  
  return(r)
}


filtro_preco_atual <- function(){
  t <- tagList(
    fluidRow( ## LINHA COM FILTROS
      column(width = 12,
             box(width = '100%',
                 fluidRow(
                   column(width = 12,
                          column(width = 2,
                                 selectInput("idUF2","UF*:", 
                                             choices = sort( unique(dados$UF) ),
                                             selected = sort( unique(dados$UF) )[1] ,#sort( unique(dados$UF) ),
                                 )
                          ),
                          column(width = 2,
                                 selectInput(inputId = "idMarca2",label = "Marca*:",
                                             choices = sort( unique(dados$MARCA) ) ),
                          ),
                          column(width = 2,
                                 selectInput(inputId = "idVeic2",label = "Veículo*:",
                                             choices = c('')  #tolower(sort( unique(dados$MODELO)) )
                                 ),
                          ),
                          column(width = 2,
                                 selectInput("idAno1",
                                             "Ano DE(opcional):",
                                             choices = c("", sort( names(metadados_filtros) ) )  
                                  )
                          ),column(width = 2,
                                 selectInput("idAno2",
                                             "Ano ATÉ(opcional):",
                                             choices = c("", sort( names(metadados_filtros) ) )
                                 )
                          ),
                      ),
                    ),
                    fluidRow(
                      column(width = 12,
                        column(width = 2,
                               selectInput("idKm1",
                                            "KM De (opcional):",
                                            choices = c("",seq(0,150000,5000) )  #sort( unique(dados$ANO))
                               )
                         ),
                         column(width = 2,
                                selectInput("idKm2",
                                             "Até KM (opcional):",
                                             choices = c("",seq(0,150000,5000) )  #sort( unique(dados$ANO))
                                )
                         ),
                        column(width = 2,
                          #actionButton("idExecuta2","Consultar",class = 'btn-success')
                          actionButtonStyled('idExecuta2', label = 'Consultar', 
                                             type = 'default',disabled=TRUE)
                          
                        )
                      )
                  )
              ) ## FIM BOX
      )
    )
  )
  return(t)
}


dash_preco_atual <- function(){
  t <- tagList(
    div(id = 'dashAtual',
        fluidRow(
        infoBoxOutput('qtd',width = 3),
        infoBoxOutput('menopreco',width = 3),
        infoBoxOutput('mediapreco',width = 3),
        infoBoxOutput('maxpreco',width = 3)
      ),
      fluidRow( 
        column(width = 12,
               box(width = '100%',
                   column(width = 6, plotlyOutput("grafico_boxplot_preco2") ),
                   column(width = 6, plotlyOutput("grafico_boxplot_km2") )
               )       
        )
      ), #FIM PRIMEIRA LINHA GRAFICOS
      fluidRow(
        column(width = 12,
               box(width = '100%',
                   column(width = 6, plotlyOutput("grafico_km_valor2") ),
                   column(width = 6, plotlyOutput("grafico_km_valor_ano2") )
               ) 
        )
      ), # FIM SEGUNDA LINHA GRÁFICOS
      fluidRow(
        column(width = 12,
               box(width = '100%',
                   column(width = 4, plotlyOutput("grafico_tipo2") ),
                   column(width = 8, plotlyOutput("grafico_tipo_valor2") ),
                   #column(width = 3, plotlyOutput("grafico_cambio2") ),
                   #column(width = 2, plotlyOutput("grafico_combus2") )
               ) 
        )
      ),  # FIM TERCEIRA LINHA GRÁFICOS
      fluidRow(
        column(width = 12,
               box(width = '100%',status = 'warning',
                   dataTableOutput("tabelaDados")
               )
        )
      )
    )
  ) ## FIM TAGLIST
  return(t)
}


infos <- function(){
  t <- tagList(
    fluidRow( 
      column(width = 12,
             box(width = '100%',
                 title = "O que é?", status = "primary", solidHeader = TRUE,
                 column(width = 6, 
                        h2('Dashboard Histórico'),
                        p("Esse módulo apresenta os históricos dos preços de venda dos automóveis"),
                        h2('Dashboard Preços atuais'),
                        p("Esse módulo apresenta os preços atuais de venda dos automóveis"),
                        p("É feito uma coleta via webscraping no momento da consulta, 
                          portanto, a execução pode demorar um pouco.")
                    )
                 )
      )
    ), 
    infoBox(title = '',subtitle = 'Instagram',
            value = a('@info_dados', href="https://www.instagram.com/info_dados"),
            color = 'orange',icon = icon('fa-brands fa-instagram') ), 
    
    infoBox(title = '',subtitle = 'Email',
            value = 'gabriel.lg08@gmail.com',
            color = 'navy',icon = icon('fa-solid fa-envelope') ),
    
    infoBox(title = '',subtitle = 'Linkedin',
            value = a('Gabriel Lima', href="https://www.linkedin.com.br/gabriellimagomes"),
            color = 'green',icon = icon('fa-solid fa-envelope') )
  )
  return(t)
}











