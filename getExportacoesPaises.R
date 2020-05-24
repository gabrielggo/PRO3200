deflator <- function(t){
  
  d = deflator_table = read.csv('API_NY.GDP.DEFL.ZS_DS2_en_csv_v2_1070444.csv', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
  reporter = t[1 , 1]
  
  dv <- d[d[1] == reporter, 33:63]
  
  #A próxima parte existe para corrigir os 'buracos'com valores NA 
  NonNAindex = which(!is.na(dv[1,]))
  firstNonNA = min(NonNAindex)
  lastNonNA = max(NonNAindex)
  
  cagr = (dv[1, lastNonNA]/dv[1, firstNonNA])^(1/(lastNonNA - firstNonNA)) #Inflação histórica média
  
  
  if(firstNonNA != 1){
    dv[1, 1] = dv[1, lastNonNA]/(cagr^(lastNonNA - 1)) #Garante que o primeiro valor não é nulo
  }
  
  for(i in 1:length(dv)){
    if(is.na(dv[1, i])){
      dv[1, i] = dv[1, i - 1]*cagr #Assume a inflação histórica média para os anos em que não há dados
    }
  }
  
  dv = rev(dv)
  dv[1,] = dv[1,]/dv[1,1]
  
  for(i in 1:nrow(t)){
    t[i, 6:36] = t[i, 6:36]/dv
  }
  
  return(t)
}

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
    
    if(teste$p.value <= 0.1){
      print(growth_data[i, 3])
      print(teste)
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

# QUINTO PASSO: TESTE HIPOTESE

tHipoteseCrescimento(ale_)