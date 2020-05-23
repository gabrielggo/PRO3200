getExportacoesPaises <- function(trade_data){
  
  #Retorna só os dados de exportação (absolutos)
  
  return(trade_data[(trade_data[['Indicator']] == 'Export(US$ Mil)')|(trade_data[['Indicator']] == 'Exports (in US$ Mil)'), ])
}


crescimentoAnual <- function(trade_data){
  
  #converte os dados absolutos ano a ano para crescimento logarítmico
  
  for(i in 1:nrow(trade_data)){
    
    trade_data[i, 6:35] = log(trade_data[i, 6:35]/trade_data[i, 7:36])
  }
  
  return(trade_data)
}

tHipoteseCrescimento <- function(growth_data){
  
  for(i in 1:nrow(growth_data)){
    
    teste = t.test(growth_data[i, 6:35], growth_data[21, 6:35])
    
    if(teste$p.value >= 0.95 | teste$p.value <= 0.05){
      print(growth[i, 3])
      print(teste$p.value)
      print('')
    }
    
    
  }
}



#teste
setwd('F:/Google Drive/Privado/Faculdade/Estatística 2020/PRO3200')

trade = read.csv('wits_en_trade_summary_allcountries_allyears/en_USA_AllYears_WITS_Trade_Summary.CSV', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
exports = getExportacoesPaises(trade)


#####################################################################
# EXEMPLO DE UTILIZAÇÃO

# PRIMEIRO PASSO (PUXAR DADOS DO PAÍS | EX: ALEMANHA)

alemanha = read.csv('wits_en_trade_summary_allcountries_allyears/en_DEU_AllYears_WITS_Trade_Summary.CSV', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)

# SEGUNDO PASSO: PEGAR SOMENTE DADOS DE EXPORTAÇAO

alemanha_exp = getExportacoesPaises(alemanha)

# TERCEIRO PASSO: AJUSTAR PELA INFLAÇÃO 

ale_exp_aj = deflator(alemanha_exp)

# QUARTO PASSO: TRANSFORMAR EM CRESCIMENTO ANUAL

ale_crs = crescimentoAnual(ale_exp_aj)