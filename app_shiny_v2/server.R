
shinyServer(function(input, output) {
  
  dados_select <- eventReactive(input$idExecuta,{
    #modelo <- 'corolla xei 16v'
    #uf <- c('mt','se')
    
    print(input$idOpcionais)
    
    resultado <- dados %>% 
      filter(MODELO == input$idModelos & 
               UF %in% input$idUf)
    
    for (c in input$idOpcionais) {
      resultado <- resultado %>%
        filter_at(vars(c), all_vars(. == 1))
    }
    resultado
    
  },ignoreNULL = F)
  
  output$teste <- renderText({
    paste('DADOS:', nrow(dados_select())) 
  })
  
  output$grafico_media_valores <- renderPlotly({
    ## VALOR MÉDIO AO LONGO TEMPO
    mediana_data <- dados_select() %>%
      group_by(DATA_COLETA_METADADOS,UF) %>%
      summarise(mediaValor = median(VALOR),.groups = 'drop')
    
    ggplotly(
      mediana_data %>%
        ggplot() +
        geom_line(aes(x = DATA_COLETA_METADADOS, y = mediaValor,
                      group = UF, color = UF),
                  size = 1) + ggtitle("Média Valor") +
        scale_color_brewer(palette = 'Dark2')
    )
  })
  
  output$grafico_boxplot_preco <- renderPlotly({
    ggplotly(
      dados_select() %>%
        ggplot() + 
        geom_boxplot(aes(x = UF, y = VALOR, fill = UF))+
        ggtitle('Variação de Preço por UF')+
        theme(legend.position = 'none') +
        scale_fill_brewer(palette = 'Dark2')
    )
  })
  
  output$grafico_km_valor <- renderPlotly({
    ggplotly( dados_select() %>% 
                ggplot()+
                geom_point(aes(x = QUILOMETRAGEM, y = VALOR,color = UF) )+
                ggtitle("Distribuição do VALOR e KM por UF")+
                scale_color_brewer(palette="Dark2")
    )
  })
  
  output$grafico_tipo_anuncio <- renderPlotly({
    ggplotly(dados_select() %>% 
               ggplot(aes(x = TIPO_ANUNCIO, y = VALOR,fill = UF))+
               geom_boxplot( ) +
               ggtitle("Variação do Preço por Tipo de Anúncio")+
               scale_fill_brewer(palette="Dark2")
    )%>% layout(boxmode='group')
  })
  
  output$grafico_pie_cambio <- renderPlotly({
    freq_cambio <- dados_select() %>% 
      group_by(CÂMBIO) %>%
      summarise(qtd = n()) %>%
      mutate(prop = qtd / sum(qtd) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )
    
    plot_ly(freq_cambio, labels = ~CÂMBIO, values = ~prop  , type = 'pie',
            textinfo = 'label+percent',showlegend = FALSE) %>%
      layout(title = 'Quantidade por Câmbio')
  })
  
  output$grafico_pie_direcao <- renderPlotly({
    freq_direcao <- dados_select() %>% 
      group_by(DIREÇÃO) %>%
      summarise(qtd = n()) %>%
      mutate(prop = qtd / sum(qtd) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )
    
    plot_ly(freq_direcao, labels = ~DIREÇÃO, values = ~prop  , type = 'pie',
            textinfo = 'label+percent',showlegend = FALSE) %>%
      layout(title = 'Quantidade por Direção')
  })
  
  output$grafico_bar_cor <- renderPlotly({ggplotly(
    dados_select() %>%
      group_by(COR) %>%
      summarise(QTD = n() ) %>%
      ggplot() +
      geom_bar(aes(x = reorder(COR,QTD ),y = QTD,fill=QTD),stat = 'identity' )+
      ggtitle("Quantidade por Cor") + xlab('COR')
  )
  })
  
})
