
source('global.R')
source('scrap_olx.R')

cabecalho <- dashboardHeader(title = 'VENDAS AUTOMÓVEIS')

barra_lateral <- dashboardSidebar(
    sidebarMenu(id = 'menu',
        menuItem("Dashboard Histórico", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Preços Atuais", tabName = "precoAtual", icon = icon("th")),
        menuItem("Infos", tabName = "infos", icon = icon("info"))
    ),
    useShinyjs()
  )

corpo_pagina <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",   
       fluidRow(
        infoBox(title = '',subtitle = 'Registros',
                value = format( nrow(dados),big.mark=".", decimal.mark = ','),
                color = 'navy',icon = icon('database') ),
        
        infoBox(title = '',subtitle = 'Modelos Disponíveis',
                value = length(unique(dados$MODELO)),color = 'blue',
                icon = icon('car')),
        
        infoBox(title = '',subtitle = 'UFs',
                value = length(unique(dados$UF)),color = 'navy',
                icon = icon('location-dot') )
        
      ), # FIM LINHA BOXES INFORMATIVOS
      
      fluidRow( ## LINHA COM FILTROS
        column(width = 12,
               box(width = '100%',
                  fluidRow(
                     column(width = 12,
                            column(width = 2,
                                   selectInput(inputId = "idMarca",label = "Marca:",
                                               choices = sort( unique(dados$MARCA) ) ),
                            ),
                            column(width = 2,
                                   selectInput(inputId = "idVeic",label = "Veículo:",
                                               choices = c('')  #tolower(sort( unique(dados$MODELO)) )
                                               ),
                            ),
                            column(width = 2,
                                   #selectInput(inputId = "idModelos",label = "Modelo:",choices = c('')  #sort( unique(dados$MODELO_ORIGIN)) )
                                   selectInput("idPot","Potência:", 
                                               choices = c('')  #sort( unique(dados$POTENCIA) )
                                               )
                            ),
                            column(width = 2,
                                   selectInput("idAno",
                                               "Ano do automóvel:",
                                               choices = c('')  #sort( unique(dados$ANO))
                                   )
                            ),
                            column(width = 2,
                              selectizeInput("idUF","UF:", 
                                                  choices = c(''),  #sort( unique(dados$UF) ),
                                                  options = list(maxItems = 2),
                                                  #selected = 'df'
                                            )
                            ),
                     )
                   ),
                  fluidRow(
                    column(width = 12,
                          column(width = 12,
                                 checkboxGroupInput("idOpcionais","Opcionais",
                                                    choices = colnames(dados)[seq(27,36)],
                                                    inline = T  )),
                          
                          #actionButton("idExecuta","Consultar",class = 'btn-success')
                          actionButtonStyled('idExecuta', label = 'Consultar', 
                                             type = 'success',disable=TRUE)
                    )
                  )
               )## FIM BOX
        )
      ), # FIM LINHA DE FILTROS
      div(id = 'msg',
          fluidRow(
            column(width = 12,
              box(width = '100%',status='warning',
                  valueBox(width = '100%',subtitle = ' ',
                      value ='SELECIONE OS VALORES NOS FILTROS
                      PARA GERAR OS GRÁFICOS',color = 'yellow',icon = icon('circle-info') )
                )
            )
          )
      ),
       div(id = 'graficos',
          fluidRow(
            column(width = 12,
                   box(width = '100%',
                       column(width = 8, plotlyOutput("grafico_media_valores") ),
                       column(width = 4, plotlyOutput("grafico_boxplot_preco") )
                   )       
            )
          ), #FIM PRIMEIRA LINHA GRAFICOS
          fluidRow(
            column(width = 12,
                   box(width = '100%',
                       column(width = 5, plotlyOutput("grafico_km_valor") ),
                       column(width = 4, plotlyOutput("grafico_tipo_anuncio") ),
                       column(width = 3, plotlyOutput("grafico_pie_cambio") )
                   ) 
            )
          ), # FIM SEGUNDA LINHA GRÁFICOS
          fluidRow(
            column(width = 12,
                   box(width = '100%',
                       column(width = 3,plotlyOutput("grafico_pie_direcao") ),
                       column(width = 9,plotlyOutput("grafico_bar_cor") )
                   ) 
            )
          )# FIM TERCEIRA LINHA GRÁFICOS
        )  # FIM DIV GRAFICOS
      
    ), # FIM DO DASHBOARDBODY
    
    # Second tab content
    tabItem(tabName = "precoAtual",
        uiOutput('dashPrecoAtual'),
        uiOutput('graficosPrecoAtual')
    ),
    tabItem(tabName = "infos",
            uiOutput('info')
    )
    
    
  )## FIM TABITEMS
  
)

dashboardPage(header = cabecalho,
              sidebar = barra_lateral,
              body = corpo_pagina)

