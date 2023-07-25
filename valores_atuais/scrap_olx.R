
library(rvest)
library(stringr)
library(tidyverse)
library(httr)
#pag <- 1
#re <- 1 #data superior
#rs <- 41 #data inferior

#ms <- 5000 #km inferior
#me <- 5000 #km superior

#Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/27.0.1453.93 Safari/537.36
#Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36

httr::set_config(
  httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36"))

#httr::config('user_agent')
#options("HTTPUserAgent")
#getOption("HTTPUserAgent")
#get_user_agent()


#t <- GET(url)
#t$request$options$useragent
#tt <- content(t,'text')
#tt <- minimal_html(tt)
#lista_anuncios = tt %>% html_elements("ul#ad-list")
#lista_anuncios = lista_anuncios %>% html_elements("a")

#uf <- 'df'
#marca <- 'hyundai'
#modelo <- 'hb20'
#pag <- 1
#https://df.olx.com.br/autos-e-pecas/carros-vans-e-utilitarios/vw-volkswagen/gol?re=21&rs=20&me=&ms=
url <- "https://df.olx.com.br/autos-e-pecas/carros-vans-e-utilitarios/vw-volkswagen/gol?re=21&rs=20&me=&ms=" 
  #"https://df.olx.com.br/autos-e-pecas/carros-vans-e-utilitarios/fiat/uno"
ano_inicio <- "2002"
ano_fim <- "2003"
km_fim <- ""
km_inicio <- ""

scrap_olx <- function(uf, marca, modelo, ano_inicio , ano_fim ,
                      km_inicio, km_fim ){
  marca <- str_replace_all(marca,"\\s+","")
  
  df_final <- data.frame()
  
  for (pag in seq(1,1) ){
    url <- paste0("https://",uf,
                  ".olx.com.br/autos-e-pecas/carros-vans-e-utilitarios/",
                  marca,"/",modelo)
    
    if(ano_inicio != "" & ano_fim == ano_inicio){
      url <- paste0(url,"/",ano_inicio)
      result_get <- GET(url,query = list(me = km_fim,ms = km_inicio) ) 
    }else{
      result_get <- GET(url,query = list(re = metadados_filtros[[ano_fim]],
                                         rs = metadados_filtros[[ano_inicio]],
                                         me = km_fim,ms = km_inicio) )
    }
    print(result_get$request$url)
    
    ### CODIGO TEMPORARIO PORQUE OLX ESTÁ MUDANDO A PÃGINA
    # AS VEZES LER TODAS AS INFOS, AS VEZES NÃO
    pagina <- content(result_get,'text')
    pagina <- minimal_html(pagina)
    
    # CAPTURANDO LISTA DE ANUNCIOS
    lista_anuncios = pagina %>% html_elements("ul#ad-list>li")
    #lista_anuncios = lista_anuncios %>% html_elements("li")
    
    links   = c()
    titulos = c()
    precos  = c()
    kms  = c()
    anos = c()
    combust  = c()
    cambio   = c()
    df_final = data.frame()
    
    i <- 1
    #anuncio <- lista_anuncios[i]
    #anuncio %>% html_text()
    
    for(anuncio in lista_anuncios){
      if (anuncio %>% html_attr("class") != "sponsored" & 
          grepl("R\\$",anuncio %>% html_text2()) ){
        #print(anuncio %>% html_text() )
        
        links <- c(links, anuncio %>% html_elements("a.horizontal") %>% 
                     html_attr("href") )
        
        # CAPTURANDO LISTA DE TITULOS DOS ANUNCIOS
        titulos <- c(titulos, anuncio %>% html_elements("h2") %>% html_text() )
        
        # CAPTURANDO LISTA DE PRECOS DOS ANUNCIOS
        #xpath="//h3[@class='horizontal  price sc-ifAKCX bytyxL']"
        precos <- c(precos, anuncio %>% html_element('.horizontal.price.sc-ifAKCX.bytyxL') %>%
                      html_text2() %>% #.[i] %>% 
                      str_replace_all("R\\$ ","") %>%
                      str_replace_all("\\.","") %>% as.numeric() )
        
        # CAPTURANDO LISTA DE INFOS(KM, ANO, COMBUSTIVEL E CAMBIO)
        #"//ul[@data-testid='labelGroup']"
        #lista_infos <- 
        lista_infos <-    anuncio %>%
          html_elements("ul>li>span") %>% html_text2() #%>% paste(.,collapse = '\n') %>% str_split("\n")
        
        kms <- c(kms, as.numeric(str_replace_all("\\.|\\s+|km","",string = lista_infos[1]) )  )
        anos <- c(anos, as.numeric( str_replace_all("\\s+","",string = lista_infos[2]) )  )
        combust <- c(combust, str_replace_all("\\s+","",string = lista_infos[3])  )
        cambio <- c(cambio, str_replace_all("\\s+","",string = lista_infos[4]) )
        
        #print(paste(i, length(titulos), length(precos), length(kms), 
        #           length(anos), length(combust), length(cambio) ))
        i <- i + 1
      }  
    }
    
    # CRIANDO DF FINAL
    df <- data.frame(titulos,precos,links,kms,anos,cambio,combust)
    df_final <- rbind(df_final,df)
    df_final <- df_final %>% filter(precos > 0)
    Sys.sleep(time = 2)
  }
  return(df_final)
}



