
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  #shinyjs::toggle(id = "graficos")
  #outputOptions(output, 'graficos')

  
  #observe({
  #  #shinyjs::hide(id = "graficos")
  #  #shinyjs::disable("idExecuta")
  #  x <- dados %>% filter(  MARCA == input$idMarca )
  #  updateSelectInput(session,inputId =  "idVeic",
  #                    label = 'Veículo:',
  #                    choices = unique(x$MODELO) ) 
  #})
  #
  #
  #observeEvent(input$idVeic, ignoreInit = TRUE, {
  #  #x <- dados %>% filter(grepl(paste0('\\b',input$idVeic,'\\b'), MODELO_ORIGIN))
  #  x <- dados %>% filter(input$idVeic == MODELO )
  #  
  #  modelos <- c("")    
  #  potencia <- c("")
  #  
  #  if( nrow(x) > 0){
  #    potencia <- sort( unique(x$POTENCIA))
  #    ufs <- sort( unique(x$UF))
  #    modelos <- sort( unique(x$MODELO_ORIGIN))
  #  }
  #  updateSelectInput(session,"idModelos","Modelo:", 
  #                    choices = modelos
  #  )
  #  
  #  updateSelectInput(session,"idPot","Potência:", 
  #                    choices = potencia
  #  )
  #  
  #})
  #
  #observeEvent(input$idPot, ignoreInit = TRUE, {
  #  print(input$idPot)
  #  #x <- dados %>% filter(MODELO_ORIGIN == input$idModelos)
  #  x <- dados %>% filter(MODELO == input$idVeic & 
  #                          POTENCIA == input$idPot)
  #  ano <- c("")
  #  print(nrow(x))
  #  if( nrow(x) > 0){
  #    ano <- sort( unique(x$ANO))
  #  }
  #  updateSelectInput(session,"idAno","Ano do automóvel:", 
  #                    choices = sort(unique(x$ANO)))
  #   
  #})
  
  observeEvent(input$idAno, ignoreInit = TRUE, {
    #x <- dados %>% filter(MODELO_ORIGIN == input$idModelos & ANO == input$idAno)
    x <- dados %>% filter(MODELO  == input$idVeic &
                            ANO == input$idAno & POTENCIA == input$idPot)
    
    ufs <- c("")
    if( nrow(x) > 0){
      ufs <- sort( unique(x$UF))
      #shinyjs::enable("idExecuta")
      
    }
    updateSelectInput(session,"idUF","UF:", 
                      choices = ufs,
                      selected = ufs[1]
                      )
  })
   
   
  
  #eventReactive(input$idExecuta, {
  dados_select <- eventReactive( input$idExecuta , {
     
    selecionados <- dados %>% 
                      filter(MODELO  == input$idVeic & 
                               ANO == input$idAno & 
                               POTENCIA == input$idPot & 
                               UF %in% input$idUF
                      )
    
    for (c in input$idOpcionais) {
      cat(c,' - ')
      selecionados <- selecionados %>%
        filter_at(vars(c), all_vars(. == 1))
    }
    
    print(nrow(selecionados))
    if(nrow(selecionados) == 0){
      
      #shinyjs::
      hide(id = "graficos",anim = T)
      #shinyjs::
      show(id = "msg",anim = T)
      
      print("REMOVE")
      #removeUI(selector = '#graficos')
      
    }else{
      shinyjs::show(id = "graficos",anim = T)
      shinyjs::hide(id = "msg",anim = T)
      
      cat('\nSELECAO:',input$idUF,input$idModelos,
          input$idAno,nrow(selecionados))
      
      #removeUI(selector = paste0("#", "rowLabel"))
      
      #insertUI(selector = "#placeholder",where = "afterEnd",ui = novoElemento()) 
      
      selecionados
       
    }
  },ignoreNULL  = T)
   
  output$idInfoQtd <- renderInfoBox({
    valueBox(width = 12,
             value = nrow(dados_select()),
             subtitle = 'Veículos:')
  })
  
  output$grafico_media_valores <- renderPlotly({
    #cat('\nMEDIA VALORES')
    mediana_data <- dados_select() %>% 
      group_by(DATA_COLETA_METADADOS,UF) %>%
      summarise(medianaValor = median(VALOR))
    
    ggplotly(mediana_data %>% 
               ggplot() +
               geom_line(aes(x = DATA_COLETA_METADADOS, y = medianaValor, 
                             color = UF,group=UF),
                         size=1) +
               ggtitle("Média de Valor")+
               scale_color_brewer(palette="Dark2")
    )
    
  })
  
  output$grafico_boxplot_preco <- renderPlotly({
    #cat('\ngrafico_boxplot_preco')
    ggplotly(dados_select() %>%
               ggplot()+
               geom_boxplot(aes(x = UF, y = VALOR,fill = UF) ) +
               theme(legend.position="none")+
               ggtitle("Variação do Preço por UF")+
               scale_fill_brewer(palette="Dark2")
    )
  })
  
  output$grafico_km_valor <- renderPlotly({
    # cat('\n grafico_km_valor')
    ggplotly( dados_select() %>% 
                ggplot()+
                geom_point(aes(x = QUILOMETRAGEM, y = VALOR,color = UF) )+
                ggtitle("Distribuição do VALOR e KM por UF")+
                scale_color_brewer(palette="Dark2")
    )
  })
  
  output$grafico_tipo_anuncio <- renderPlotly({
    # cat('\n grafico_tipo_anuncio')
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
    # frequencia por DIREÇÃO(GRÁFICO DE PIZZA) ####
    freq_direcao <- dados_select() %>% 
      group_by(DIREÇÃO) %>%
      summarise(qtd = n()) %>%
      mutate(prop = qtd / sum(qtd) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )
    
    plot_ly(freq_direcao, labels = ~DIREÇÃO, values = ~prop  , type = 'pie',
            textinfo = 'label+percent',showlegend = FALSE) %>%
      layout(title = 'Quantidade por Direção')
  })
  
  output$grafico_bar_cor <- renderPlotly({
    ggplotly(
      dados_select() %>%
        group_by(COR) %>%
        summarise(QTD = n() ) %>%
        ggplot() +
        geom_bar(aes(x = reorder(COR,QTD ),y = QTD,fill=QTD),
                 stat = 'identity' )+
        theme(legend.position = 'none')+
        ggtitle("Quantidade por Cor") + xlab('COR')
    )
  })

  
####### SERVER DASH PREÇO ATUAL ####
  observe({
      output$dashPrecoAtual <- renderUI({
        filtro_preco_atual()
      })
      output$graficosPrecoAtual <- renderUI({
        dash_preco_atual()
      })
  })
  
  observeEvent(input$idMarca2,ignoreInit =T,{
      x <- dados %>% filter(  MARCA == input$idMarca2 )
      updateSelectInput(session,inputId =  "idVeic2",
                        label = 'Veículo*:',
                        choices = sort(unique(tolower(x$MODELO)) ) )
      
  })
  observeEvent(input$idVeic2,{
    print(paste("VEICU",input$idVeic2 ))
    if (input$idVeic2 != ''){
      updateActionButtonStyled(session, 'idExecuta2',type = 'success',
                               disabled = FALSE)
    }
    if (input$idVeic2 == ''){
      updateActionButtonStyled(session, 'idExecuta2',type = 'default',
                               disabled = T)
    }
  })
  
                             
  observeEvent(input$idExecuta2,ignoreNULL =T,{
      print("EXECUTA 2")
      
      resulta_scrap <- scrap_olx(input$idUF2, input$idMarca2, input$idVeic2,
                                 input$idAno1,input$idAno2,
                                 input$idKm1,input$idKm2)
      #resulta_scrap <- resulta_scrap %>% replace(is.na(.),0)
      
      qtdLinhas = nrow(resulta_scrap)
      if (qtdLinhas > 0){
        print(qtdLinhas)
        shinyjs::hide(id = "msg",anim = T)
      }else{
        shinyjs::show(id = "msg",anim = T)
      }
      output$tabelaDados <- renderDataTable( 
        resulta_scrap,options = list(pageLength = 10)
      )
      output$qtd <- renderInfoBox( 
        infoBox(title = '',subtitle = 'QTD Anúncios',
                value = nrow(resulta_scrap),color = 'navy',icon = icon('car') )
      )
      
      output$menopreco <- renderInfoBox( 
        infoBox(title = '',subtitle = 'Menor Preço',
                value = format( min(resulta_scrap$precos,na.rm = T),
                                big.mark=".",decimal.mark = ','),
                color = 'green',icon = icon('sack-dollar'))
      )
      output$mediapreco <- renderInfoBox( 
        infoBox(title = '',subtitle = 'Preço Médio',
                value = format(median(resulta_scrap$precos,na.rm = T),
                               big.mark=".",decimal.mark = ','),
                color = 'yellow',icon = icon('sack-dollar'))
      )
      output$maxpreco <- renderInfoBox( 
        infoBox(title = '',subtitle = 'Maior Preço',
                value = format(max(resulta_scrap$precos,na.rm = T),
                               big.mark=".",decimal.mark = ','),
                color = 'red',icon = icon('sack-dollar') )
      )
      
      output$grafico_boxplot_preco2 <- renderPlotly({
        ggplotly(resulta_scrap %>%
                   ggplot()+
                   geom_boxplot(aes(y = precos,fill = 'blue'),
                                outlier.fill =  'red') +
                   theme(legend.position="none",
                         axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())+
                   ggtitle("Variação do Preço")+
                   scale_fill_brewer(palette="Dark2")
        )
      })
      output$grafico_boxplot_km2 <- renderPlotly({
          ggplotly(resulta_scrap %>%
                     ggplot()+
                     geom_boxplot(aes(y = kms,fill = 'blue'),
                                  outlier.fill =  'red') +
                     theme(legend.position="none",
                           axis.title.x=element_blank(),
                           axis.text.x=element_blank(),
                           axis.ticks.x=element_blank())+
                     ggtitle("Variação de KM")+
                     scale_fill_manual(values = brewer.pal(n = 8, 
                                       name = "Dark2")[8]
                                       )
          )
      })
      
      
      output$grafico_km_valor2 <- renderPlotly({
        # cat('\n grafico_km_valor')
        ggplotly( resulta_scrap %>% 
                    ggplot()+
                    geom_point(aes(x = kms, y = precos,color='red',
                                   text = paste('KM: ',format( kms,big.mark=".",
                                                               decimal.mark = ','),
                                    '<br>Preço:', format( precos,big.mark=".",
                                                          decimal.mark = ',')
                                    )
                                   ) 
                               ) +
                    theme(legend.position="none")+
                    ggtitle("Distribuição do VALOR e KM")+
                    scale_color_manual(values = brewer.pal(n = 8, 
                                                           name = "Dark2")[1]),
                  tooltip = c("text")
        )
      })
      
      output$grafico_km_valor_ano2 <- renderPlotly({
        # cat('\n grafico_km_valor')
        ggplotly( resulta_scrap %>% 
                    ggplot()+
                    geom_point(aes(x = kms, y = precos,
                                   color = as.factor(anos),
                                   text = paste('KM: ',format( kms,big.mark=".",
                                                              decimal.mark = ','),
                                          '<br>Preço:', format( precos,big.mark=".",
                                                               decimal.mark = ','),
                                          "<br>Ano:",anos)
                                 ) 
                               ) +
                    ggtitle("Distribuição do VALOR, KM e Ano ")+
                    scale_color_brewer(palette="Dark2") + 
                    guides(color = guide_legend(title = "Ano")),
                  #theme(legend.title = element_text('Ano') ),
              tooltip = c("text")
        )
      })  
      
      output$grafico_tipo2 <- renderPlotly({
        freq_tipo <- resulta_scrap %>% 
          group_by(tipo) %>%
          summarise(qtd = n()) %>%
          mutate(prop = qtd / sum(qtd) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
        
        plot_ly(freq_tipo, labels = ~tipo, values = ~prop, 
                type = 'pie', textinfo = 'label+percent',
                showlegend = FALSE) %>%
          layout(title = ' TIPO ANÚNCIO')
      }) 
      
      output$grafico_tipo_valor2 <- renderPlotly({
        ggplotly(resulta_scrap %>%
                   ggplot()+
                   geom_boxplot(aes(x = tipo, y = precos,
                                    fill = tipo),
                                outlier.fill =  'red') +
                   theme(legend.position="none")+
                   ggtitle("Variação Preço por TIPO ANÚNCIO")+
                   scale_fill_brewer(palette="Set2")
        )
      })
      
      output$grafico_cambio2 <- renderPlotly({
        freq_cambio <- resulta_scrap %>% 
          group_by(cambio) %>%
          summarise(qtd = n()) %>%
          mutate(prop = qtd / sum(qtd) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
        
          ggplotly(
            freq_cambio %>%
              ggplot() + geom_bar(aes(cambio, prop),stat = 'identity')
          )
      })  
       
      output$grafico_combus2 <- renderPlotly({
        freq_comb <- resulta_scrap %>% 
          group_by(combust) %>%
          summarise(qtd = n()) %>%
          mutate(prop = qtd / sum(qtd) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
        
        plot_ly(freq_comb, labels = ~combust, values = ~prop, 
                type = 'pie', textinfo = 'label+percent',
                showlegend = FALSE) %>%
          layout(title = 'Quantidade por Combustível')
      }) 
  })
  
  #### SERVER INFOS ####
  # monta o menu de INFORMAÇÕES sobre a página(app)
  output$info <- renderUI({
    infos()
  })
  
  print("FIM")
  
})
