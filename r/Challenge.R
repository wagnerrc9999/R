
  # https://transparencia.tce.sp.gov.br/apis
  

    library(rjson)
    library(jsonlite) 
    library(readr) 
    library(dplyr) 
    library(tidyr)
    library(magrittr) 
    library(stringr) 
    library(lubridate) 
    library(tibble)
  
install.packages("httr")
library(httr)
install.packages("jsonlite")
library(jsonlite)
install.packages("readr")
library(readr)

  ## buscar os dados
  
  # lista de municipios
  dados_municipios <- fromJSON(
    paste(
      readLines( "https://transparencia.tce.sp.gov.br/api/json/municipios" ),
      collapse = "" )
  )
  
  dados_municipios %<>% 
    as_tibble()
  
  
  # dados despesas
  dados_despesas <- fromJSON(
    paste(
      readLines( "https://transparencia.tce.sp.gov.br/api/json/despesas/balsamo/2015/1" ),
      collapse = "" )
  )
  
  dados_despesas %<>% 
    as_tibble() %>% 
    mutate(dt_emissao_despesa = dmy(dt_emissao_despesa),
           vl_despesa = as.numeric(gsub(",", ".", gsub("\\.", "", vl_despesa))) )
  
  dados_despesas %>% colnames()
  
  write_csv(dados_despesas, 'dados_despesas.csv')
  
  # dados receitas
  dados_receitas <- fromJSON(
    paste(
      readLines( "https://transparencia.tce.sp.gov.br/api/json/receitas/balsamo/2015/1" ),
      collapse = "" )
  )
  
  dados_receitas %<>% 
    as_tibble() %>% 
    mutate(ano = 2015,
           vl_arrecadacao = as.numeric(gsub(",", ".", gsub("\\.", "", vl_arrecadacao))) )
  
  dados_receitas %>% colnames()
  
  write_csv(dados_receitas, 'dados_receitas.csv')
  
    
  