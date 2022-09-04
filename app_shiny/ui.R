
source('util.R')

shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "idModelos",label = "Modelo:",
                        choices = sort( unique(dados$MODELO) ),
                        selected = 'corolla xei 16v'),
            
            selectizeInput("idUf","UF:", 
                           choices = sort( unique(dados$UF) ),
                           options = list(maxItems = 3),
                           selected = 'df'),
            sliderInput("idAno",
                        "Ano do automóvel:",
                        min = min(dados$ANO),
                        max = max(dados$ANO),
                        value = 2022,sep = ""),
            checkboxGroupInput("idOpcionais","Opcionais",
                               choices = colnames(dados)[seq(27,36)],
                               inline = T  ),
            
            actionButton("idExecuta","Consultar")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("teste"),
            plotlyOutput("grafico_media_valores"),
            plotlyOutput("grafico_boxplot_preco"),
            plotlyOutput("grafico_km_valor"),
            plotlyOutput("grafico_tipo_anuncio"),
            plotlyOutput("grafico_pie_cambio"),
            plotlyOutput("grafico_pie_direcao"),
            plotlyOutput("grafico_bar_cor")
        )
    )
))
