#--------------------------------------------------------------------------------------------
#---------------------------------- MÓDULOS NECESSÁRIOS -------------------------------------



#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- PERSONALIZAÇÃO RAPIDA ----------------------------------

# ELEMENTOS QUE DEVEM SER ALTERADOS MANUALMENTE PARA A EXECUÇÃO DO PROGRAMA EM QUALQUE MICRO   



#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------------------- CLASSES E FUNÇÕES ---------------------------------------

#--------------------------------------------------------------------------------------------
        
#--------------------------------------------------------------------------------------------
#---------------------------------------- MAIN ----------------------------------------------

#Criação das dataframes do modelo de dados
countries <- data.frame(
  "country_id" =c(1),
  "country_code" = c(1), 
  "country_name" = c(1), 
  stringsAsFactors = FALSE
)

country_relationships <- data.frame(
  "relationship_id" = c(1),
  "country1_id" = c(1),
  "country2_id" = c(2),
  stringsAsFactors = FALSE
)

country_indicators <- data.frame(
  "country_indicator_id" = c(1),
  "country_indicator_code" = c(1),
  "country_indicator_name" = c(1),
  stringsAsFactors = FALSE
)

relationship_indicators <- data.frame(
  "relationship_indicator_id" = c(1),
  "relationship_indicator_code" = c(1),
  "relationship_indicator_name" = c(1),
  stringsAsFactors = FALSE
)

country_indicator_series <- data.frame(
  "data_id" = c(1),
  "country_id" = c(1),
  "country_indicator_id" = c(1),
  "data_time" = c(1),
  "data_value" = c(1),
  stringsAsFactors = FALSE
)

relationship_indicator_series <- data.frame(
  "data_id" = c(1),
  "relationship_id" = c(1),
  "relationship_indicator_id" = c(1),
  "data_time" = c(1),
  "data_value" = c(1),
  stringsAsFactors = FALSE
)

#--------------------------------------------------------------------------------------------
        

        