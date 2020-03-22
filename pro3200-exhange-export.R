#--------------------------------------------------------------------------------------------
#---------------------------------- MÓDULOS NECESSÁRIOS -------------------------------------
#garantir que esses módulos estão instalados para funcionamento correto da aplicação

library(reshape2)
library(qdapTools)
library(plyr)

#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- PERSONALIZAÇÃO RAPIDA ----------------------------------

# ELEMENTOS QUE DEVEM SER ALTERADOS MANUALMENTE PARA A EXECUÇÃO DO PROGRAMA EM QUALQUE MICRO   

data_directory <- "F:/Google Drive/Privado/Faculdade/Estatística 2020/PRO3200" #Diretório que contém os dados 


#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- CLASSES E FUNÇÕES ---------------------------------------

#to-do: Função que passa os dados sobre taxa de câmbio para o modelo de dados

#to-do: função que passa os dados sobre comércio para o modelo de dados
sq_trade_data <- function(source, 
                          mcountries = countries, 
                          mcountry_relationships = country_relationships, 
                          mcountry_indicators = country_indicators, 
                          mrelationship_indicators = relationship_indicators, 
                          mcountry_indicator_series = country_indicator_series, 
                          mrelationship_indicator_series = relationship_indicator_series){
  
  for(i in 1:nrow(source)){
    
    #confere se a coluna "reporter" é um país novo
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
    
    reporter_id <- mcountries[mcountries[["country_name"]] == source[i,1], 1]
    
    
    if(source[i,2] != '...'){ #quando a linha trata de dados entre dois países (exportação, importação, etc)
      
      
      #confere se partner é um país novo
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
      partner_id <- mcountries[mcountries[['country_name']] == source[i, 2], 1]
      
      #confere se o relacionamento reporter/partner é novo
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

      indicator_name <- paste(source[i, 3], source[i, 4], source[i, 5]) #nome dos indicadores tá feio -> ponto de melhoria
      
      #confere se o indicador de relacionamente é novo
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
      relationship_indicator_id <- mrelationship_indicators[mrelationship_indicators[['relationship_indicator_name']] == indicator_name, 1]
      

    }else{ #caso a linha trate de dados do país isolado
      
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
      
      country_indicator_id <- mcountry_indicators[mcountry_indicators[['country_indicator_name']] == indicator_name, 1] 
        
    }
  }
  source <- melt(source,id.vars = names(source[1:5]), variable.name = "year")
  for(i in 1:nrow(source)){
    reporter_id = lookup(source[i,1], mcountries[['country_name']], mcountries[['country_id']])
    if(source[i,2] != '...'){
      #adiciona dados à tabela de séries
      partner_id = lookup(source[i,2], mcountries[['country_name']], mcountries[['country_id']])
      ss <- mcountry_relationships[mcountry_relationships[['country1_id']]  == reporter_id, ]
      relationship_id = lookup(partner_id, ss[['country2_id']], ss[['relationship_id']])
      indicator_name <- paste(source[i,3],source[i,4],source[i,5])
      relationship_indicator_id <- lookup(indicator_name, mrelationship_indicators[['relationship_indicator_name']], mrelationship_indicators[['relationship_indicator_id']])
      mrelationship_indicator_series <- rbind(
        mrelationship_indicator_series,
        list(nrow(mrelationship_indicator_series) + 1, relationship_id, relationship_indicator_id, as.Date(source[i,6], format = "X%Y"), source[i,7])
      )
      
    }else{
      #adiciona dado na à tabela de séries de país
      indicator_name <- paste(source[i,4], source[i,5])
      country_indicator_id = lookup(indicator_name, mcountry_indicators[['country_indicator_name']], mcountry_indicators[['country_indicator_id']])
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


#to-do função que plota a time series de um indicador de país
plot_c_indicator <- function(country_id, indicator_id, count = countries, mcountry_indicator_series = country_indicator_series,
                             mcountry_indicators = country_indicators){ #o conceito é esse, mas depois a gente tem que fazer mudanças estéticas 
  plot(
    x = mcountry_indicator_series[(mcountry_indicator_series[['country_id']] == country_id)&(mcountry_indicator_series[['country_indicator_id']] == indicator_id), 4],
    y = mcountry_indicator_series[(mcountry_indicator_series[['country_id']] == country_id)&(mcountry_indicator_series[['country_indicator_id']] == indicator_id), 5],
    main = lookup(indicator_id, mcountry_indicators[1], mcountry_indicators[2]),
    type = "l",
    xlab = NULL,
    ylab = "Millions of US$", #mudar depois
    col = 'cornflowerblue'
  )
}

#to-do função que plota a time series de um indicador de relacionamento
plot_r_indicator <- function(rel_id, 
                             ind_id, 
                             con = countries, 
                             rel = country_relationships, 
                             rel_ind = relationship_indicator_series,
                             rel_ind_series = relationship_indicator_series){ #o conceito é esse, mas depois a gente tem que fazer mudanças estéticas 
  plot(
    x = rel_ind_series[(rel_ind_series[['relationship_id']] == rel_id)&(rel_ind_series[['relationship_indicator_id']] == ind_id), 4],
    y = rel_ind_series[(rel_ind_series[['relationship_id']] == rel_id)&(rel_ind_series[['relationship_indicator_id']] == ind_id), 5],

  )
}


#to-do função que calcula a variação anual de um indicador
cgrowth <- function(dfx){
  
  dfx <- ddply(dfx,.(country_id, country_indicator_id),transform,
               annual_growth =c(NA,exp(diff(log(data_value)))-1))
  return(dfx[['annual_growth']])
  
}

rgrowth <- function(dfx){
  
  dfx <- ddply(dfx,.(relationship_id, relationship_indicator_id),transform,
               annual_growth =c(NA,exp(diff(log(data_value)))-1))
  return(dfx[['annual_growth']])
  
}

#to-do função que correlaciona indicador A no período n e indicador B no período (n - x)
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

#coleta todos os dados sobre exchange
files <- list.files('dados_1afase', full.names = TRUE)

for(i in files){
  print(i)
  ext <- sq_trade_data(read.csv(i), countries, 
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


#maiores economias
biggest_economies <- get_biggest_economies() #lista com todos países que em algum momento estiveram entre as cinco maiores

#médias de exportação
mean_exports <- get_mean_exports(biggest_economies) #tabela 5 linhas e 3: 1a países, 2a médias exportacao, 3a desvio padrao da media

#5 maiores exportadores
fivebiggest <- calc_5biggest(mean_exports) #tabela com 5 linhas 3 colunas: 1a paíes, 2a medias exportacao, 3a delta


#maiores setores
biggest_sectors <- get_biggest_sectors() #lista com todos países que em algum momento estiveram entre as cinco maiores

#médias de exportação
mean_sector_exports <- get_mean_sector_exports(biggest_sectors) #tabela: 1a países, 2a médias exportacao, 3a desvio padrao da media

#5 maiores setores
fivebiggestSectors <- cal_5biggestSectors(mean_sector_exports)#tabela com 5 linhas: 1a paíes, 2a medias exportacao, 3a delta


#--------------------------------------------------------------------------------------------
#códigos teporarios para teste DELETE THIS

#Abaixo um exemplo de como passar os dados para o modelo de dados
testsource <- read.csv('wits_en_trade_summary_allcountries_allyears/en_ABW_AllYears_WITS_Trade_Summary.CSV')

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

#encontrei um pacote chamado qdapTools, com funções de lookup, que são úteis para navegar nosso modelo de dados
#abaixo, eu peguei o nome do país reporter e partner, com base no id de relacionamento
#provavelmente essa é a situação mais difícil que a gente vai passar

reporter_id <- lookup(relationship_indicator_series$relationship_id, country_relationships$relationship_id, country_relationships$country1_id)
partner_id <- lookup(relationship_indicator_series$relationship_id, country_relationships$relationship_id, country_relationships$country2_id)
reporter_name <- lookup(reporter_id, countries$country_id, countries$country_name)
partner_name <- lookup(partner_id, countries$country_id, countries$country_name)

#para plotar time series de um indicador

plot_c_indicator(
  lookup("United States", countries$country_name, countries$country_id),
  lookup("Development GDP (current US$ Mil)", country_indicators$country_indicator_name, country_indicators$country_indicator_id)
  )
 #ou

plot_c_indicator(2,2)

#time series de exportações de exportações de produtos animais (indicador 5) entre os EUA e o méxico (relacionamento 7)
plot_r_indicator(7, 5)
                              

                              
                              ######
teste <- country_indicator_series[(country_indicator_series$country_id == 12)&(country_indicator_series$country_indicator_id == 2), ],
print(teste)


##################################################################################






