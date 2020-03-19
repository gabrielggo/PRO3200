#--------------------------------------------------------------------------------------------
#---------------------------------- MÓDULOS NECESSÁRIOS -------------------------------------
#garantir que esses módulos estão instalados para funcionamento correto da aplicação

library(reshape2)

#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- PERSONALIZAÇÃO RAPIDA ----------------------------------

# ELEMENTOS QUE DEVEM SER ALTERADOS MANUALMENTE PARA A EXECUÇÃO DO PROGRAMA EM QUALQUE MICRO   

data_directory <- "F:/Google Drive/Privado/Faculdade/Estatística 2020/Dados" #Diretório que contém os dados 


#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- CLASSES E FUNÇÕES ---------------------------------------


#todo: Função que passa os dados sobre taxa de câmbio para o modelo de dados
#todo: função que passa os dados sobre comércio para o modelo de dados
sq_trade_data <- function(source, mcountries, mcontry_relationships, mcountry_indicators, mrelationship_indicators, mcountry_indicator_series, mrelationship_indicator_series){
  source <- melt(source,id.vars = names(source[1:5]), variable.name = "year")
  return(source)
}
#--------------------------------------------------------------------------------------------
        
#--------------------------------------------------------------------------------------------
#---------------------------------------- MAIN ----------------------------------------------

#Configuração de diretório de trabalho
setwd(data_directory)

#Criação das dataframes do modelo de dados
countries <- data.frame(
  "country_id" = integer(), 
  "country_name" = character(), 
  stringsAsFactors = FALSE
)

country_relationships <- data.frame(
  "relationship_id" = integer(),
  "country1_id" = integer(),
  "country2_id" = integer(),
  stringsAsFactors = FALSE
)

country_indicators <- data.frame(
  "country_indicator_id" = integer(),
  "country_indicator_name" = character(),
  stringsAsFactors = FALSE
)

relationship_indicators <- data.frame(
  "relationship_indicator_id" = integer(),
  "relationship_indicator_name" = character(),
  stringsAsFactors = FALSE
)

country_indicator_series <- data.frame(
  "data_id" = integer(),
  "country_id" = integer(),
  "country_indicator_id" = integer(),
  "data_time" = as.Date(character()),
  "data_value" = double(),
  stringsAsFactors = FALSE
)

relationship_indicator_series <- data.frame(
  "data_id" = integer(),
  "relationship_id" = integer(),
  "relationship_indicator_id" = integer(),
  "data_time" = as.Date(character()),
  "data_value" = double(),
  stringsAsFactors = FALSE
)

#--------------------------------------------------------------------------------------------
        

        