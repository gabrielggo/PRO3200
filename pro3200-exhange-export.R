#--------------------------------------------------------------------------------------------
#---------------------------------- M�DULOS NECESS�RIOS -------------------------------------
#garantir que esses m�dulos est�o instalados para funcionamento correto da aplica��o

library(reshape2)
library(qdapTools)
library(plyr)

#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- PERSONALIZA��O RAPIDA ----------------------------------

# ELEMENTOS QUE DEVEM SER ALTERADOS MANUALMENTE PARA A EXECU��O DO PROGRAMA EM QUALQUE MICRO   

data_directory <- "F:/Google Drive/Privado/Faculdade/Estat�stica 2020/PRO3200" #Diret�rio que cont�m os dados 


#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- CLASSES E FUN��ES ---------------------------------------

#to-do: Fun��o que passa os dados sobre taxa de c�mbio para o modelo de dados

#to-do: fun��o que passa os dados sobre com�rcio para o modelo de dados
sq_trade_data <- function(source, 
                          mcountries = countries, 
                          mcountry_relationships = country_relationships, 
                          mcountry_indicators = country_indicators, 
                          mrelationship_indicators = relationship_indicators, 
                          mcountry_indicator_series = country_indicator_series, 
                          mrelationship_indicator_series = relationship_indicator_series){
  
  for(i in 1:nrow(source)){
    
    #confere se a coluna "reporter" � um pa�s novo
    new_country = T
    for(j in 1:nrow(mcountries)){
      if(source[i,1] == mcountries[j,2]){
        new_country = F
        break()
      }
    }
    if(new_country){ #se for novo adiciona na tabela de pa�ses
      mcountries <- rbind(mcountries, list(nrow(mcountries) +1, source[i,1]))
    }
    
    reporter_id <- mcountries[mcountries[["country_name"]] == source[i,1], 1]
    
    
    if(source[i,2] != '...'){ #quando a linha trata de dados entre dois pa�ses (exporta��o, importa��o, etc)
      
      
      #confere se partner � um pa�s novo
      new_country = T
      for(j in 1:nrow(mcountries)){
        if(source[i,2] == mcountries[j,2]){
          new_country = F
          break()
        }
      }
      if(new_country){ #se for novo, adiciona na tabela de pa�ses
        mcountries <- rbind(mcountries, list(nrow(mcountries) +1, source[i, 2]))
      }
      partner_id <- mcountries[mcountries[['country_name']] == source[i, 2], 1]
      
      #confere se o relacionamento reporter/partner � novo
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

      indicator_name <- paste(source[i, 3], source[i, 4], source[i, 5]) #nome dos indicadores t� feio -> ponto de melhoria
      
      #confere se o indicador de relacionamente � novo
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
      

    }else{ #caso a linha trate de dados do pa�s isolado
      
      indicator_name <- paste(source[i,4], source[i,5]) #nome dos indicadores t� feio -> ponto de melhoria
      
      #confere se o indicador de pa�s � novo
      new_country_indicator = T
      for(j in 1:nrow(mcountry_indicators)){
        if(indicator_name == mcountry_indicators[j, 2]){
          new_country_indicator = F
          break()
        }
      }
      if(new_country_indicator){ #se for novo, adicionar na tabela de indicador de pa�s
        mcountry_indicators <- rbind(mcountry_indicators, list((nrow(mcountry_indicators) + 1), indicator_name))
      }
      
      country_indicator_id <- mcountry_indicators[mcountry_indicators[['country_indicator_name']] == indicator_name, 1] 
        
    }
  }
  source <- melt(source,id.vars = names(source[1:5]), variable.name = "year")
  for(i in 1:nrow(source)){
    reporter_id = lookup(source[i,1], mcountries[['country_name']], mcountries[['country_id']])
    if(source[i,2] != '...'){
      #adiciona dados � tabela de s�ries
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
      #adiciona dado na � tabela de s�ries de pa�s
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


#to-do fun��o que plota a time series de um indicador de pa�s
plot_c_indicator <- function(country_id, indicator_id, count = countries, mcountry_indicator_series = country_indicator_series,
                             mcountry_indicators = country_indicators){ #o conceito � esse, mas depois a gente tem que fazer mudan�as est�ticas 
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

#to-do fun��o que plota a time series de um indicador de relacionamento
plot_r_indicator <- function(rel_id, 
                             ind_id, 
                             con = countries, 
                             rel = country_relationships, 
                             rel_ind = relationship_indicator_series,
                             rel_ind_series = relationship_indicator_series){ #o conceito � esse, mas depois a gente tem que fazer mudan�as est�ticas 
  plot(
    x = rel_ind_series[(rel_ind_series[['relationship_id']] == rel_id)&(rel_ind_series[['relationship_indicator_id']] == ind_id), 4],
    y = rel_ind_series[(rel_ind_series[['relationship_id']] == rel_id)&(rel_ind_series[['relationship_indicator_id']] == ind_id), 5],

  )
}


#to-do fun��o que calcula a varia��o anual de um indicador
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

#to-do fun��o que correlaciona indicador A no per�odo n e indicador B no per�odo (n - x)
#--------------------------------------------------------------------------------------------
        
#--------------------------------------------------------------------------------------------
#---------------------------------------- MAIN ----------------------------------------------

#Configura��o de diret�rio de trabalho
setwd(data_directory)

#Cria��o das dataframes do modelo de dados
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

countries <- read.csv('countries.csv', header = T, sep = ',', dec = '.', stringsAsFactors = F, colClasses=c("NULL",NA,NA))
country_relationships <- read.csv('country_relationships.csv', header = T, sep = ',', dec = '.', stringsAsFactors = F,colClasses=c("NULL",NA,NA,NA))
country_indicators <- read.csv('country_indicators.csv', header = T, sep = ',', dec = '.', stringsAsFactors = F, colClasses=c("NULL",NA,NA))
relationship_indicators <- read.csv('relationship_indicators.csv', header = T, sep = ',', dec = '.', stringsAsFactors = F, colClasses=c("NULL",NA,NA))
country_indicator_series <- read.csv('country_indicator_series.csv', header = T, sep = ',', dec = '.', stringsAsFactors = F, colClasses=c("NULL",NA,NA, NA, NA, NA ))
relationship_indicator_series <- read.csv('relationship_indicator_series.csv', header = T, sep = ',', dec = '.', stringsAsFactors = F, colClasses=c("NULL",NA,NA,NA, NA, NA))

#maiores economias
biggest_economies <- get_biggest_economies() #lista com todos pa�ses que em algum momento estiveram entre as cinco maiores

#m�dias de exporta��o
mean_exports <- get_mean_exports(biggest_economies) #tabela 5 linhas e 3: 1a pa�ses, 2a m�dias exportacao, 3a desvio padrao da media

#5 maiores exportadores
fivebiggest <- calc_5biggest(mean_exports) #tabela com 5 linhas 3 colunas: 1a pa�es, 2a medias exportacao, 3a delta


#maiores setores
biggest_sectors <- get_biggest_sectors() #lista com todos pa�ses que em algum momento estiveram entre as cinco maiores

#m�dias de exporta��o
mean_sector_exports <- get_mean_sector_exports(biggest_sectors) #tabela: 1a pa�ses, 2a m�dias exportacao, 3a desvio padrao da media

#5 maiores setores
fivebiggestSectors <- cal_5biggestSectors(mean_sector_exports)#tabela com 5 linhas: 1a pa�es, 2a medias exportacao, 3a delta


#--------------------------------------------------------------------------------------------
#c�digos teporarios para teste DELETE THIS

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

#encontrei um pacote chamado qdapTools, com fun��es de lookup, que s�o �teis para navegar nosso modelo de dados
#abaixo, eu peguei o nome do pa�s reporter e partner, com base no id de relacionamento
#provavelmente essa � a situa��o mais dif�cil que a gente vai passar

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

#time series de exporta��es de exporta��es de produtos animais (indicador 5) entre os EUA e o m�xico (relacionamento 7)
plot_r_indicator(7, 5)
                              

                              
                              ######
teste <- country_indicator_series[(country_indicator_series$country_id == 12)&(country_indicator_series$country_indicator_id == 2), ],
print(teste)


##################################################################################






