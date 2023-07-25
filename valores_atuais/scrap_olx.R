
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
url <- "https://df.olx.com.br/autos-e-pecas/carros-vans-e-utilitarios/fiat/uno"
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
    
    #pagina <- read_html(url)
    
    lista_anuncios = pagina %>% html_elements("ul#ad-list")
    lista_anuncios = lista_anuncios %>% html_elements("a")
    
    x <- lista_anuncios %>% html_nodes("h3") #str_subset(regex("Preço"))
    x
    
    
    
    
    ids <- lista_anuncios %>% html_attr("data-lurker_list_id")
    titulos <- lista_anuncios %>% html_nodes("h2") %>% html_text()
    precos <- lista_anuncios %>% html_elements("span") %>% 
      html_attr("aria-label") %>% 
      str_subset(regex("Preço")) %>%
      str_replace_all("Preço do item: R\\$ ","") %>%
      str_replace_all("\\.","") %>% as.numeric()
    links <- lista_anuncios %>% html_attr("href")
    
    lista_infos <- 
      lista_anuncios %>%
      html_elements(xpath = "//div[@aria-label='Informações sobre o anúncio:']") %>%
      html_text() %>% str_replace_all("\\|\\s+$|\\s+","") %>%
      str_trim() %>% str_split(pattern = "\\|") 
     
    kms <- c()
    anos <- c()
    cambio <- c()
    combust <- c()
    
    for(x in lista_infos){
      x <- lista_infos[1]
      kms <- c(kms,as.numeric(str_replace_all("\\.|\\s+|km","",string = x[1]) ) )
      anos <- c(anos,as.numeric( str_replace_all("\\s+","",string = x[2]) ) )
      cambio <- c(cambio,str_replace_all("\\s+","",string = x[3]) )
      combust <- c(combust,str_replace_all("\\s+","",string = x[4]) )
    }
    
    tipo <- c()
    for (i in lista_anuncios %>% html_nodes(".sc-12rk7z2-15.efbgAg")){
      r = i %>% html_text() %>% str_replace("Online","")
      r = if_else(r == "",'Particular',r)
      tipo = c(tipo,  r)
    }
    
    df <- data.frame(ids,titulos,precos,links,kms,anos,cambio,combust,tipo )
    df_final <- rbind(df_final,df)
    Sys.sleep(time = 2)
  }
  return(df_final)
}
