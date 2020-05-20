#--------------------------------------------------------------------------------------------
#---------------------------------- MÓDULOS NECESSÁRIOS -------------------------------------
#garantir que esses módulos estão instalados para funcionamento correto da aplicação

library(data.table)
library(qdapTools)
library(plyr)

#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- PERSONALIZAÇÃO RAPIDA ----------------------------------

# ELEMENTOS QUE DEVEM SER ALTERADOS MANUALMENTE PARA A EXECUÇÃO DO PROGRAMA EM QUALQUE MICRO   

data_directory <- "F:/Google Drive/Privado/Faculdade/Estatística 2020/PRO3200" #Diretório que contém os dados 


#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- FUNCAO ---------------------------------------

#to-do: função que passa os dados sobre comércio para o modelo de dados
sq_trade_data <- function(source, 
                          mcountries = countries, 
                          mcountry_relationships = country_relationships, 
                          mcountry_indicators = country_indicators, 
                          mrelationship_indicators = relationship_indicators, 
                          mcountry_indicator_series = country_indicator_series, 
                          mrelationship_indicator_series = relationship_indicator_series){
  treporter <- c()
  tpartner <- c()
  trel <- c()
  trel_ind <- c()
  tc_ind <- c()
  tadd <- c()
  t0 <- Sys.time()
  
  for(i in 1:nrow(source)){
    #confere se a coluna "reporter" é um país novo
   tstart <- Sys.time()
    new_country = T
    for(j in 1:nrow(mcountries)){
      if(source[i,1] == mcountries[j,2]){
        new_country = F
        break()
      }
    }
    if(new_country){ #se for novo adiciona na tabela de países
      mcountries <- rbind(mcountries, list(nrow(mcountries) +1, source[i,1]))
    }
    
    reporter_id <- lookup(source[i,1], mcountries[['country_name']], mcountries[['country_id']])
    source[i,1] <- reporter_id
    
    tend <- Sys.time()
    treporter <- c(treporter, tend - tstart)
    
    if(source[i,2] != '...'){ #quando a linha trata de dados entre dois países (exportação, importação, etc)
      
      
      #confere se partner é um país novo
      tstart <- Sys.time()
      new_country = T
      for(j in 1:nrow(mcountries)){
        if(source[i,2] == mcountries[j,2]){
          new_country = F
          break()
        }
      }
      if(new_country){ #se for novo, adiciona na tabela de países
        mcountries <- rbind(mcountries, list(nrow(mcountries) +1, source[i, 2]))
      }
      partner_id <- lookup(source[i,2], mcountries[['country_name']], mcountries[['country_id']])
      source[i,2] <- partner_id
      tend <- Sys.time()
      tpartner <- c(tpartner, tend - tstart)
      
      #confere se o relacionamento reporter/partner é novo
      tstart <- Sys.time()
      new_relationship = T
      for(j in 1:nrow(mcountry_relationships)){
        if((reporter_id == mcountry_relationships[j, 2])&(partner_id == mcountry_relationships[j, 3])){
          new_relationship = F
          break()
        }
      }
      if(new_relationship){ #se for novo, adiciona na tabela de relacionamentos
        mcountry_relationships <- rbind(mcountry_relationships, list(nrow(mcountry_relationships) + 1, reporter_id, partner_id))
      }
      relationship_id <- mcountry_relationships[(mcountry_relationships[['country1_id']] == reporter_id)&(mcountry_relationships[['country2_id']] == partner_id), 1]
      source[i,2] < - relationship_id
      indicator_name <- paste(source[i, 3], source[i, 4], source[i, 5]) #nome dos indicadores tá feio -> ponto de melhoria
      tend <- Sys.time()
      trel <- c(trel, tend - tstart)
      
      #confere se o indicador de relacionamente é novo
      tstart <- Sys.time()
      new_relationship_indicator = T
      for(j in 1:nrow(mrelationship_indicators)){
        if(indicator_name == mrelationship_indicators[j,2]){
          new_relationship_indicator = F
          break()
        }
      }
      if(new_relationship_indicator){ #se for novo, adiciona na tabela de indicadores de relacionamento
        mrelationship_indicators <- rbind(mrelationship_indicators, list(nrow(mrelationship_indicators) + 1, indicator_name))
      }
      relationship_indicator_id <- lookup(indicator_name, mrelationship_indicators[['relationship_indicator_name']], mrelationship_indicators[['relationship_indicator_id']])
      source[i,3] <- relationship_indicator_id
      tend <- Sys.time()
      trel_ind <- c(trel_ind, tend - tstart)

    }else{ #caso a linha trate de dados do país isolado
      
      source[i,2] <- 0
      tstart <- Sys.time()
      indicator_name <- paste(source[i,4], source[i,5]) #nome dos indicadores tá feio -> ponto de melhoria
      
      #confere se o indicador de país é novo
      new_country_indicator = T
      for(j in 1:nrow(mcountry_indicators)){
        if(indicator_name == mcountry_indicators[j, 2]){
          new_country_indicator = F
          break()
        }
      }
      if(new_country_indicator){ #se for novo, adicionar na tabela de indicador de país
        mcountry_indicators <- rbind(mcountry_indicators, list((nrow(mcountry_indicators) + 1), indicator_name))
      }
      
      country_indicator_id <- lookup(indicator_name, mcountry_indicators[['country_indicator_name']], mcountry_indicators[['country_indicator_id']])
      source[i,3] <- country_indicator_id
      tend <- Sys.time()
      tc_ind <- c(tc_ind, tend - tstart)
    }
  }
  tstart <- Sys.time()
  source <- melt(setDT(source),id.vars = names(source[1:5]), variable.name = "data_time", value.name = 'data_value', variable.factor = F)%>% str()
  source <- source[-c(4,5)]
  tend <- Sys.time()
  tmelt = tend - tstart
  str(source)
  tstart <- Sys.time()
  names(source) <- c('country_id', 'relationship_id', 'country_indicator_id', 'data_time', 'data_value')
  source[4] <- as.Date.character(source['data_time'], "X%Y")
  mcountry_indicator_series <- rbind(
    mcountry_indicator_series,
    source[source[['relationship_id']] == 0, -2]
  )
  names(source) <- c('country_id', 'relationship_id', 'relationship_indicator_id', 'data_time', 'data_value')
 mrelationship_indicator_series <- rbind(
    mrelationship_indicator_series,
    source[source[['relationship_id']] != 0, -1]
  )
  tend <- Sys.time()
  tadd <- c(tadd, tend - tstart)
  
  print(paste('Reporter: ', mean(treporter), ' ', sum(treporter)))
  print(paste('Partner: ', mean(tpartner), ' ', sum(tpartner)))
  print(paste('trel: ', mean(trel)))
  print(paste('trel_ind: ', mean(trel_ind), ' ', sum(trel_ind)))
  print(paste('tc_ind: ', mean(tc_ind), ' ', sum(tc_ind)))
  print(paste('Add: ', mean(tadd), ' ', sum(tadd)))
  t1 <- Sys.time()
  print(paste('Melt: ', tmelt))
  print(paste('Total: ', t1 - t0))
  
  
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
#---------------------------------------- TESTE ----------------------------------------------

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
  "country_id" = integer(1),
  "country_indicator_id" = integer(1),
  "data_time" = as.Date(character(1), format = "%Y"),
  "data_value" = double(1),
  stringsAsFactors = FALSE
)

relationship_indicator_series <- data.frame(
  "relationship_id" = integer(1),
  "relationship_indicator_id" = integer(1),
  "data_time" = as.Date(character(1), format = "%Y"),
  "data_value" = double(1),
  stringsAsFactors = FALSE
)

#coleta todos os dados sobre exchange
files <- list.files('dados_teste', full.names = TRUE)

for(i in files){
  print(i)
  ext <- sq_trade_data(read.csv(i, header = T, sep = ',', dec = '.', stringsAsFactors = FALSE), countries, 
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
}