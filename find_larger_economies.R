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


get_mean_exports <- function(countrylist,
                 c = countries,
                 c_rel = country_relationships,
                 c_ind = country_indicators,
                 rel_ind = relationship_indicators, 
                 c_ind_series = country_indicator_series, 
                 rel_ind_series = relationship_indicator_series
){
  
  summ <- data.frame(
    'country' = character(1),
    'mean_exports' = double(1),
    'sd_exports' = double(1),
    stringsAsFactors = FALSE
  )
  #encontrar id do indicador
  ind_id = lookup('All Products Export Exports (in US$ Mil)', rel_ind[['relationship_indicator_name']], rel_ind[['relationship_indicator_id']])
  #econtrar id do mundo
  wld_id = lookup('World', c[['country_name']], c[['country_id']])
  
  for(i in countrylist){
    cname = lookup(i, c[['country_id']], c[['country_name']])
    #econtrar relacionamento
    rel_id <- c_rel[c_rel[['country1_id']] == i,]
    rel_id <- lookup(wld_id, rel_id[['country2_id']], rel_id[['relationship_id']])
    #puxar dados relevantes
    ss <- rel_ind_series[(rel_ind_series[['relationship_id']] == rel_id)&(rel_ind_series[['relationship_indicator_id']] == ind_id),]
    summ <- rbind(summ,list( cname, mean(ss[['data_value']], na.rm = TRUE), sd(ss[['data_value']], na.rm = TRUE)))

  }
  summ <- summ[-c(1),]
  return(summ)
  }  
  

write.csv(countries, file = 'countries.csv', sep = ',', dec = '.')
write.csv(country_relationships, file = 'country_relationships.csv', sep = ',', dec = '.')
write.csv(country_indicators, file = 'country_indicators.csv', sep = ',', dec = '.')
write.csv(relationship_indicators, file = 'relationship_indicators.csv', sep = ',', dec = '.')
write.csv(country_indicator_series, file = 'country_indicator_series.csv', sep = ',', dec = '.')
write.csv(relationship_indicator_series, file = 'relationship_indicator_serires.csv', sep = ',', dec = '.')


library(xlsx)
library(lubridate)

files <- list.files('dados_1afase', full.names = TRUE)

