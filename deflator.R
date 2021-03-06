setwd('F:/Google Drive/Privado/Faculdade/Estat�stica 2020/PRO3200')

trade = read.csv('wits_en_trade_summary_allcountries_allyears/en_Wld_AllYears_WITS_Trade_Summary.CSV', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)

deflator <- function(t){
  
  d = read.csv('API_NY.GDP.DEFL.ZS_DS2_en_csv_v2_1070444.csv', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)

  reporter = t[1 , 1]
  reporter = trimws(reporter)
  if(reporter == 'World'){
    reporter = 'United States'
  }

    dv <- d[d[1] == reporter, 33:63]

  #A pr�xima parte existe para corrigir os 'buracos'com valores NA 
  NonNAindex = which(!is.na(dv[1,]))
  firstNonNA = min(NonNAindex)
  lastNonNA = max(NonNAindex)
  
  cagr = (dv[1, lastNonNA]/dv[1, firstNonNA])^(1/(lastNonNA - firstNonNA)) #Infla��o hist�rica m�dia

  
  if(firstNonNA != 1){
    dv[1, 1] = dv[1, lastNonNA]/(cagr^(lastNonNA - 1)) #Garante que o primeiro valor n�o � nulo
  }
  
  for(i in 1:length(dv)){
    if(is.na(dv[1, i])){
      dv[1, i] = dv[1, i - 1]*cagr #Assume a infla��o hist�rica m�dia para os anos em que n�o h� dados
    }
  }
  
  dv = rev(dv)
  dv[1,] = dv[1,]/dv[1,1]
  
  for(i in 1:nrow(t)){
    t[i, 6:36] = t[i, 6:36]/dv
  }
  
  return(t)
}


write.csv(trade2, 'tabela_mundo_corrigida.CSV', row.names = FALSE)
