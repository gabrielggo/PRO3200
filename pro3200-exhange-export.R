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
sq_trade_data <- function(source, mcountries, mcountry_relationships, mcountry_indicators, mrelationship_indicators, mcountry_indicator_series, mrelationship_indicator_series){
  source <- melt(source,id.vars = names(source[1:5]), variable.name = "year")
  for(i in 1:nrow(source)){
    print(paste('iteração: ', i))
    new_country = T
    new_relationship = T
    new_country_indicator = T
    new_relationship_indicator = T
    
    #confere se reporter é um país novo
    for(j in 1:nrow(mcountries)){
      if(source[i,1] == mcountries[j,2]){
        new_country = F
        break()
      }
    }
    
    if(new_country){
      mcountries <- rbind(mcountries, list(nrow(mcountries) +1, source[i,1]))
    }
    reporter_id <- mcountries[mcountries[["country_name"]] == source[i,1], 1]
    if(source[i,2] != '...'){
      new_country = T
      
      #confere se partner é um país novo
      for(j in 1:nrow(mcountries)){
        if(source[i,2] == mcountries[j,2]){
          new_country = F
          break()
        }
      }
      if(new_country){
        mcountries <- rbind(mcountries, list(nrow(mcountries) +1, source[i, 2]))
      }
      partner_id <- mcountries[mcountries[['country_name']] == source[i, 2], 1]
      
      #confere se o relacionamento reporter/partner é novo
      for(j in 1:nrow(mcountry_relationships)){
        if((reporter_id == mcountry_relationships[j, 2])&(partner_id == mcountry_relationships[j, 3])){
          new_relationship = F
          break()
        }
      }
      if(new_relationship){
        mcountry_relationships <- rbind(mcountry_relationships, list(nrow(mcountry_relationships) + 1, reporter_id, partner_id))
      }
      relationship_id <- mcountry_relationships[(mcountry_relationships[['country1_id']] == reporter_id)&(mcountry_relationships[['country2_id']] == partner_id), 1]

      indicator_name <- paste(source[i, 3], source[i, 4], source[i, 5])
      
      #confere se o indicador de relacionamente é novo
      for(j in 1:nrow(mrelationship_indicators)){
        if(indicator_name == mrelationship_indicators[j,2]){
          new_relationship_indicator = F
          break()
        }
      }
      if(new_relationship_indicator){
        mrelationship_indicators <- rbind(mrelationship_indicators, list(nrow(mrelationship_indicators) + 1, indicator_name))
      }
      relationship_indicator_id <- mrelationship_indicators[mrelationship_indicators[['relationship_indicator_name']] == indicator_name, 1]
      
      #adiciona dados à tabela de series
      mrelationship_indicator_series <- rbind(
        mrelationship_indicator_series,
        list(nrow(mrelationship_indicator_series) + 1, relationship_id, relationship_indicator_id, as.Date(source[i,6], format = "X%Y"), source[i,7])
      )
    }else{
      
      indicator_name <- paste(source[i,4], source[i,5])
      
      #confere se o indicador de país é novo
      for(j in 1:nrow(mcountry_indicators)){
        if(indicator_name == mcountry_indicators[j, 2]){
          new_country_indicator = F
          break()
        }
      }
      if(new_country_indicator){
        mcountry_indicators <- rbind(mcountry_indicators, list((nrow(mcountry_indicators) + 1), indicator_name))
      }
      
      country_indicator_id <- mcountry_indicators[mcountry_indicators[['country_indicator_name']] == indicator_name, 1] 
        
      #adiciona dado na à tabela de séries de país
      mcountry_indicator_series <- rbind(
        mcountry_indicator_series,
        list((nrow(mcountry_indicator_series) + 1), reporter_id, country_indicator_id, as.Date(source[i, 6], format = "X%Y"), source[i, 7])
      )
    }
    }
  

  return(
    list(
     mcountries, 
     mcountry_relationships, 
     mcountry_indicators, 
     mrelationship_indicators, 
     mcountry_indicator_series, 
     mrelationship_indicator_series
     )
    )
}
#--------------------------------------------------------------------------------------------
        
#--------------------------------------------------------------------------------------------
#---------------------------------------- MAIN ----------------------------------------------

#Configuração de diretório de trabalho
setwd(data_directory)

#Criação das dataframes do modelo de dados
countries <- data.frame(
  "country_id" = integer(1), 
  "country_name" = character(1), 
  stringsAsFactors = FALSE
)

country_relationships <- data.frame(
  "relationship_id" = integer(1),
  "country1_id" = integer(1),
  "country2_id" = integer(1),
  stringsAsFactors = FALSE
)

country_indicators <- data.frame(
  "country_indicator_id" = integer(1),
  "country_indicator_name" = character(1),
  stringsAsFactors = FALSE
)

relationship_indicators <- data.frame(
  "relationship_indicator_id" = integer(1),
  "relationship_indicator_name" = character(1),
  stringsAsFactors = FALSE
)

country_indicator_series <- data.frame(
  "data_id" = integer(1),
  "country_id" = integer(1),
  "country_indicator_id" = integer(1),
  "data_time" = as.Date(character(1), format = "%Y"),
  "data_value" = double(1),
  stringsAsFactors = FALSE
)

relationship_indicator_series <- data.frame(
  "data_id" = integer(1),
  "relationship_id" = integer(1),
  "relationship_indicator_id" = integer(1),
  "data_time" = as.Date(character(1), format = "%Y"),
  "data_value" = double(1),
  stringsAsFactors = FALSE
)

#--------------------------------------------------------------------------------------------
#códigos teporarios para teste DELETE THIS
testsource <- read.csv('wits_en_trade_summary_allcountries_allyears/en_USA_AllYears_WITS_Trade_Summary.CSV')

ext <- sq_trade_data(testsource, countries, 
                     country_relationships, 
                     country_indicators, 
                     relationship_indicators, 
                     country_indicator_series, 
                     relationship_indicator_series
                     )

countries <- ext[[1]]
country_relationships <- ext[[2]]
country_indicators <- ext[[3]]
relationship_indicators <- ext[[4]]
country_indicator_series <- ext[[5]]
relationship_indicator_series <- ext[[6]]



        