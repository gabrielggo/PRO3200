get_biggest_economies <- function(c_indicators = country_indicators, c_ind_series = country_indicator_series){
  id_gdp = lookup('Development GDP (current US$ Mil)', c_indicators[['country_indicator_name']], c_indicators[['country_indicator_id']])
  first_ite = T
  economies <- c()
  for(i in 1988:2018){
    print(i)
    #selecionar os valores de GDP do ano correspondente
    gdp <- c_ind_series[(c_ind_series[['country_indicator_id']] == id_gdp)&(year(c_ind_series[['data_time']]) == i),]
    gdp <- gdp[order(gdp$data_value, decreasing = TRUE),]
    gdp <- gdp[1:6,]
  if(first_ite){
    first_ite = FALSE
  }else{
    for(j in 1:nrow(gdp)){
      new_country = T
      for(k in economies){
        if(gdp[j, 2] == k){
          new_country = F
          break()
        }
      }
      if(new_country){
        economies <- append(economies, gdp[j, 2])
      }
    }
    
  }
  
  }
  return(economies)
}
  
  


library(xlsx)
library(lubridate)

files <- list.files('dados_1afase', full.names = TRUE)

