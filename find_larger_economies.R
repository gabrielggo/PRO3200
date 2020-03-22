find_larger_economies(y, c_indicators = country_indicators, c_ind_series = country_indicator_series){
  # recebe um ano como argumento obrigatório
  # retorna um vetor de comprimento cinco com o id das cinco maiores economias naquele ano 
  
  #encontrar id do gdp
  id_gdp = lookup('Development GDP (current US$ Mil)', c_indicators[['country_indicator_name']], c_indicators[['country_indicator_id']])
  
  #selecionar os valores de GDP do ano correspondente
  gpd <- country_indicator_series[(country_indicator_series[['country_indicator_id']] == id_gdp)&(year(country_indicator_series[['data_time']]) == y),]
  gdp <- order(gdp[['data_value']])
  gdp <- gpd[1:5]
  return(gdp[['country_id']])
}


find_markets(y, i, c = countries, rel = country_relationships, rel_ind_series = relationship_indicator_series){
  #Recebe um ano e o id de um indicador (exportação de um determinado produto)
  #Retorna o total e
  
  id_mundo = lookup('World', c[['country_name']], c[['country_id']])
  id_mundo_mundo = rel[(rel[['countr1_id']] == id_mundo)&(rel[['country2_id']] == id_mundo)]
  
}

library(xlsx)

write.xlsx(relationship_indicators, 'F:/Google Drive/Privado/Faculdade/Estatística 2020/PRO3200/relationship_indicators.xlsx')