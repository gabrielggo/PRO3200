library(gmodels)

deflator <- function(t){
  
  d = deflator_table = read.csv('API_NY.GDP.DEFL.ZS_DS2_en_csv_v2_1070444.csv', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
  reporter = t[1 , 1]
  
  dv <- d[d[1] == 'United States', 33:63]
  
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

getProductshare <- function(trade_data){
  
  #Retorna só os dados de composição da exportação (relativos)
  
  return(trade_data[(trade_data[['Indicator']] == 'Export Product share(%)'), ])
}

crescimentoAnual <- function(trade_data){
  
  #converte os dados absolutos ano a ano para crescimento logarítmico
  
  for(i in 1:nrow(trade_data)){

    trade_data[i, 6:35] = log(as.numeric(trade_data[i, 6:35])/as.numeric(trade_data[i, 7:36]))
  }
  
  return(trade_data)
}

tHipoteseCrescimento <- function(growth_data){
  
  print(growth_data[16, 3])
  for(i in 1:nrow(growth_data)){
    print(i)
    teste = t.test(as.numeric(growth_data[i, 6:35]), as.numeric(growth_data[16, 6:35]))
    
    if(teste$p.value >= 0.1){
      print(growth_data[i, 3])
      print(teste)
      print('')
    }
    
    
  }
}

agrega_menores_setores <- function(trade_data){
  setorespequenos = c('Animal', 'Footwear', 'Hides and Skins', 'Minerals', 'Vegetables')
  
  menores_setores = trade_data[
    trade_data[['Product.categories']] == ('Animal')|
      trade_data[['Product.categories']] == ('Footwear')|
      trade_data[['Product.categories']] == ('Hides and Skins')|
      trade_data[['Product.categories']] == ('Minerals')|
      trade_data[['Product.categories']] == ('Vegetables')|
      trade_data[['Product.categories']] == ('Miscellaneous'),
  ]
  
  trade_data <- trade_data[
    trade_data[['Product.categories']] != ('Animal')&
      trade_data[['Product.categories']] != ('Footwear')&
      trade_data[['Product.categories']] != ('Hides and Skins')&
      trade_data[['Product.categories']] != ('Minerals')&
      trade_data[['Product.categories']] != ('Vegetables')&
      trade_data[['Product.categories']] != ('Miscellaneous'),
    ]
  novalinha = colSums(menores_setores[,6:36])
  
  trade_data <- rbind(trade_data, c(trade_data[1,1], trade_data[1,2], 'Other Categories', trade_data[1,4], trade_data[1,5], novalinha))
  return(trade_data)
}

tRegressao_setor <- function(td){
  
  setores <- c()
  
  anos = c(2018:1988)
  anos_depois = c(2018:2008)
  anos_antes = c(2007:1988)
  
  historico_estimativa <- c()
  historico_inf <- c()
  historico_sup <- c()
  
  depois_estimativa <- c()
  depois_inf <- c()
  depois_sup <- c()
  
  antes_estimativa <- c()
  antes_inf <- c()
  antes_sup <- c()
  
  beta_estimativa <- c()
  beta_inf <- c()
  beta_sup <- c()
  
  allExports_hist = as.numeric(td[16, 6:36])
  
 
  for(i in 1:nrow(td)){
    
    setores[i] <- td[i, 3]
    
    setor_hist = as.numeric(td[i, 6:36])
    setor_depois = as.numeric(td[i, 6:16])
    setor_antes = as.numeric(td[i, 17:36])
    
    historico = cor.test(setor_hist, anos, conf.level = 0.9)
    depois = cor.test(setor_depois, anos_depois, conf.level = 0.9)
    antes = cor.test(setor_antes, anos_antes, conf.level = 0.9)
    beta = cor.test(setor_hist, as.numeric(td[16, 6:36]), conf.level = 0.9)
    
    historico_estimativa[i] <- historico$estimate
    historico_inf[i] <- historico$conf.int[1]
    historico_sup[i] <- historico$conf.int[2]
    
    depois_estimativa[i] <- depois$estimate
    depois_inf[i] <- depois$conf.int[1]
    depois_sup[i] <- depois$conf.int[2]
    
    antes_estimativa[i] <- antes$estimate
    antes_inf[i] <- antes$conf.int[1]
    antes_sup[i] <- antes$conf.int[2]
    
    beta_estimativa[i] <- beta$estimate
    beta_inf[i] <- beta$conf.int[1]
    beta_sup[i] <- beta$conf.int[2]
  }
  
  correlacoes <- data.frame(
    setores,
    historico_estimativa,
    historico_inf,
    historico_sup,
    depois_estimativa,
    depois_inf,
    depois_sup,
    antes_estimativa,
    antes_inf,
    antes_sup,
    beta_estimativa,
    beta_inf,
    beta_sup,
    stringsAsFactors = FALSE
  )
  
  return(correlacoes)
}


#teste
setwd('F:/Google Drive/Privado/Faculdade/Estatística 2020/PRO3200')

trade = read.csv('wits_en_trade_summary_allcountries_allyears/en_USA_AllYears_WITS_Trade_Summary.CSV', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
exports = getExportacoesPaises(trade)


#####################################################################
# EXEMPLO DE UTILIZAÇÃO

# PRIMEIRO PASSO (PUXAR DADOS DO PAÍS | EX: ALEMANHA)

pais = read.csv('wits_en_trade_summary_allcountries_allyears/en_CHN_AllYears_WITS_Trade_Summary.CSV', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)

# SEGUNDO PASSO: PEGAR SOMENTE DADOS DE EXPORTAÇAO

pais = getExportacoesPaises(pais)

# TERCEIRO PASSO: AJUSTAR PELA INFLAÇÃO 

pais = deflator(pais)

#
pais = agrega_menores_setores(pais)

# QUARTO PASSO: TRANSFORMAR EM CRESCIMENTO ANUAL

pais = crescimentoAnual(pais)

# QUINTO PASSO: TESTE HIPOTESE

tHipoteseCrescimento(pais)



