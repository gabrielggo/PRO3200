setwd('F:/Google Drive/Privado/Faculdade/Estatística 2020/PRO3200')

trade = read.csv('wits_en_trade_summary_allcountries_allyears/en_Wld_AllYears_WITS_Trade_Summary.CSV', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
deflator_table = read.csv('API_NY.GDP.DEFL.ZS_DS2_en_csv_v2_1070444.csv', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)

deflator <- function(t, d){
  
  reporter = t[1 , 1]

    dv <- d[d[1] == reporter, 33:63]
    print(dv)

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

