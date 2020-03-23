# Funcao que devolve uma lista com todos os setores de exportacao que 
# alguma vez nos ultimos 30 anos esteve entre os 4 que mais exportaram em dolar

get_biggest_sectors <- function() {
  biggest_export_sectors = c()
  # Carrega a tabela com os dados:
  export_data_table = read.csv('./wits_en_trade_summary_allcountries_allyears/custon_Table_AllYears_Sectors_Trade.csv',
                               header = TRUE, sep = ';')
  
  # varre as colunas (todos os anos)
  for(j in 2:32){
    year_values = c()
    year_values = export_data_table[ , j]
    # encontra os 5 maiores valores em um ano
    biggest_5_values = tail(sort(year_values), 5)
    
    # encontra os paises cujo valor de exportacao foi maior que o menor dos maiores valores neste ano
    for (i in 1:20) {
      if(export_data_table[i, j] >= biggest_5_values[1]){

        # Varre o array de resposta para nao adicionar valores repetidos
        should_add = TRUE
        for(val in biggest_export_sectors){
          if(val == as.character(export_data_table[i, 1])){
            should_add = FALSE
            break()
          }
        }
        if(should_add){
          # Adiciona o valor caso ainda nao tenha
          biggest_export_sectors = append(biggest_export_sectors, as.character(export_data_table[i, 1]))
        }

      }
    }
  }
  
  return(biggest_export_sectors)
}
