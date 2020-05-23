getExportacoesPaises <- function(trade_data){
  
  #Retorna só os dados de exportação (absolutos)
  
  return(trade_data[(trade_data[['Indicator']] == 'Export(US$ Mil)')|(trade_data[['Indicator']] == 'Exports(US$ Mil)'), ])
}


#Ainda fazendo
crescimentoAnual <- function(trade_data){
  
  #converte os dados absolutos ano a ano para crescimento logarítmico
  
  for(i in 1:nrow(trade_data)){
    
    trade_data[i] = log(df[2:length(df[,1]), 'close']/df[1:(length(df[,1]) - 1), 'close']
  }
  log(df[2:length(df[,1]), 'close']/df[1:(length(df[,1]) - 1), 'close']
}

#teste
setwd('F:/Google Drive/Privado/Faculdade/Estatística 2020/PRO3200')

trade = read.csv('wits_en_trade_summary_allcountries_allyears/en_USA_AllYears_WITS_Trade_Summary.CSV', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
exports = getExportacoesPaises(trade)
