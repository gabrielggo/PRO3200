files <- list.files('dados_1afase', full.names = TRUE)
a = c('wits_en_trade_summary_allcountries_allyears/en_USA_AllYears_WITS_Trade_Summary.CSV',
      'wits_en_trade_summary_allcountries_allyears/en_CHN_AllYears_WITS_Trade_Summary.CSV',
      'wits_en_trade_summary_allcountries_allyears/en_JPN_AllYears_WITS_Trade_Summary.CSV', 
      'wits_en_trade_summary_allcountries_allyears/en_DEU_AllYears_WITS_Trade_Summary.CSV', 
      'wits_en_trade_summary_allcountries_allyears/en_FRA_AllYears_WITS_Trade_Summary.CSV'
      )

for(i in a){
  pais = read.csv(i, header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
  print(pais[1,1])
  pais = getExportacoesPaises(pais)
  print('getExportacoes ok')
  pais = deflator(pais)
  print('deflator k')
  pais = agrega_menores_setores(pais)
  print('agrega ok')
  pais = crescimentoAnual(pais)
  print('calculo do crescimento ok')
  path = paste('C:/Users/André/Desktop/', pais[1,1], '.CSV')
  #b = pais[21,6:35]
  #boxplot(b)
  write.csv(pais, path, row.names = FALSE)
}
