library(plyr)

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


################################################################################
#Utilizaçao
#Exemplo: variação anual da exportação de produtos químicos dos EUA para o Mundo
#Exportação de produtos químicos: id = 7
#Id EUA = 2, ID mundo = 11
#Id relacionamento enre EUA e mundo = 10


ss =   relationship_indicator_series[
  (relationship_indicator_series[['relationship_id']] == 10)
  &(relationship_indicator_series[['relationship_indicator_id']] == 7),
  ]

crescimento <- rgrowth(ss)

plot(
  x = ss$data_time,
  y = crescimento,
  type = 'l'
)
