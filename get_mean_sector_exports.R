# Pega os maiores setores de exportação
source("get_biggest_sectors.R")
biggest_sectors = get_biggest_sectors()
# Carrega a tabela com os dados:
export_data_table = read.csv('./wits_en_trade_summary_allcountries_allyears/custon_Table_AllYears_Sectors_Trade.csv',
                             header = TRUE, sep = ';')

get_mean_sector_exports <- function(biggest_sectors){
  # Create an enpty matrix
  mean_desvpad_table = matrix(data = NA, nrow = length(biggest_sectors), ncol = 3)
  colnames(mean_desvpad_table) <- c('Country', 'Mean', 'DesvPad')
  # Put biggest sectors in col 1
  mean_desvpad_table[ , 1] = biggest_sectors
  
  # Get mean and desv pad values for each sector
  for(i in 1:length(biggest_sectors)){
    for(j in 1:length(export_data_table[ , 1])){
      if(as.character(mean_desvpad_table[i , 1]) == as.character(as.matrix(export_data_table[j, 1]))){

        # calculate Average value and puts it on the table
        mean_desvpad_table[i, 2] = mean(as.numeric(export_data_table[j, 2:32]))
        
        # calculate Standart Deviation and puts it on the table
        mean_desvpad_table[i, 3] = sd(as.numeric(export_data_table[j, 2:32]))
        
      }
    }
  }
  
  return(mean_desvpad_table)
}
