setwd('F:/Google Drive/Privado/Faculdade/Estatística 2020/PRO3200') #ajustar de acordo com seus sistema

#FUNÇOES NECESSÁRIAS

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


#######################################################

files <- list.files('dados_1afase', full.names = TRUE)
a = c('wits_en_trade_summary_allcountries_allyears/en_USA_AllYears_WITS_Trade_Summary.CSV',
      'wits_en_trade_summary_allcountries_allyears/en_CHN_AllYears_WITS_Trade_Summary.CSV',
      'wits_en_trade_summary_allcountries_allyears/en_JPN_AllYears_WITS_Trade_Summary.CSV', 
      'wits_en_trade_summary_allcountries_allyears/en_DEU_AllYears_WITS_Trade_Summary.CSV', 
      'wits_en_trade_summary_allcountries_allyears/en_FRA_AllYears_WITS_Trade_Summary.CSV'
      )


#Cotelar os dados da fonte

chn_source = read.csv('wits_en_trade_summary_allcountries_allyears/en_CHN_AllYears_WITS_Trade_Summary.CSV',
                header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
usa_source = read.csv('wits_en_trade_summary_allcountries_allyears/en_USA_AllYears_WITS_Trade_Summary.CSV',
                header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
deu_source = read.csv('wits_en_trade_summary_allcountries_allyears/en_DEU_AllYears_WITS_Trade_Summary.CSV',
                header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
jpn_source = read.csv('wits_en_trade_summary_allcountries_allyears/en_JPN_AllYears_WITS_Trade_Summary.CSV',
                header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
fra_source = read.csv('wits_en_trade_summary_allcountries_allyears/en_FRA_AllYears_WITS_Trade_Summary.CSV',
                header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)

#Recortes necessários
usa_abs = getExportacoesPaises(usa_source)
usa_rel= getProductshare(usa_source)

chn_abs = getExportacoesPaises(chn_source)
chn_rel= getProductshare(chn_source)

jpn_abs = getExportacoesPaises(jpn_source)
jpn_rel= getProductshare(jpn_source)

deu_abs = getExportacoesPaises(deu_source)
deu_rel= getProductshare(deu_source)

fra_abs = getExportacoesPaises(fra_source)
fra_rel= getProductshare(fra_source)


#deflaciona os dados absolutos
usa_abs = deflator(usa_abs)
chn_abs = deflator(chn_abs)
jpn_abs = deflator(jpn_abs)
deu_abs = deflator(deu_abs)
fra_abs = deflator(fra_abs)

#junta os menores setores
usa_abs = agrega_menores_setores(usa_abs)
usa_rel = agrega_menores_setores(usa_rel)

chn_abs = agrega_menores_setores(chn_abs)
chn_rel = agrega_menores_setores(chn_rel)

jpn_abs = agrega_menores_setores(jpn_abs)
jpn_rel = agrega_menores_setores(jpn_rel)

deu_abs = agrega_menores_setores(deu_abs)
deu_rel = agrega_menores_setores(deu_rel)

fra_abs = agrega_menores_setores(fra_abs)
fra_rel = agrega_menores_setores(fra_rel)

#calcula crescimento anual(dados absolutos)
usa_cres = crescimentoAnual(usa_abs)
chn_cres = crescimentoAnual(chn_abs)
jpn_cres = crescimentoAnual(jpn_abs)
deu_cres = crescimentoAnual(deu_abs)
fra_cres = crescimentoAnual(fra_abs)


#PRIMEIRA ANÁLISE: REGRESSÃO LINEAR DESVIO PADRÃO DO PRODUCT EXPORT SHARE
#MOTIVO: SE HOUVER MUDANÇA NO DESVIO PADRÃO PODEMOS AFIRMAR QUE A COMPOSIÇÃO MUDOU
anos = c(2018:1988)
anosA = c(2007:1988)
anosD = c(2018:2008)

desvios = c()
  
for(j in 6:36){
  desvios[j - 5] <- sd(fra_rel[,j])  #alterar 'usa' pela sigla do pais que queremos analisar
}

plot(x = anos, y = desvios, type = 'b', main ='França: evolução do desvio padrão da composição das exportações',
     pch = 19, xlab = '', ylab = 'Desvio padrão', col = 'royalblue') #TODO: Melhorar a estética desse gráfico para colocar no relatório
  
teste1 = lm(desvios~anos)
abline(teste1, col = 'firebrick')
  
summary(teste1) #Resultado: podemos afirmar mudança da composição de exportação de todos países menos França

#SEGUNDA ANÁLISE: REGRESSÃO LINEAR DO PRODUCT SHARE DE TODOS SETORES
#MOTIVO: JÁ IDENTIFICAMOS QUE A COMPOSIÇÃO MUDOU, AGORA VAMOS IDENTIFICAR QUAIS SETORES MUDARAM

exp_rel = rbind(usa_rel, chn_rel, jpn_rel, deu_rel)



for(i in 1:nrow(usa_rel)){
  
  str = usa_rel[i, 3]
  
  productShare = c(usa_rel[i, 6:36], chn_rel[i, 6:36], jpn_rel[i, 6:36], deu_rel[i, 6:36])
  anos4 = c(anos, anos, anos, anos)
  
  plot(y = productShare,
       x = anos4,
       main = paste(str,': participação nas exportações das quatro maiores economias entre 1988 e 2018'),
       xlab = '',
       ylab = 'Participação (%)',
       col = 'springgreen',
       pch = 19)
       
  
  print(str)
  
  teste2 = lm(as.numeric(productShare)~anos4)
  abline(teste2, col = 'navyblue')
  
  print(summary(teste2))
}





#TERCEIRA E ÚLTIMA ANÁLISE: SEPARAR ENTRE ANTES E DEPOIS DE 2008
#MOTIVAÇÃO: NA ANÁLISE EXPLORATÓRIA, OBSERVAMOS UMA MUDANÇA DE COMPORTAMENTO DEPOIS DE 2008
# E VAMOS VERIFICAR SE TEVE EFEITO NA COMPOSIÇÃO DE EXPORTAÇOES


#ANTES DE 2008
for(i in 1:nrow(usa_rel)){
  
  str = usa_rel[i, 3]
  
  productShare = c(usa_rel[i, 17:36], chn_rel[i, 17:36], jpn_rel[i, 17:36], deu_rel[i, 17:36])
  anos4 = c(anosA, anosA, anosA, anosA)
  
  plot(y = productShare,
       x = anos4,
       main = paste(str,': participação nas exportações das quatro maiores economias entre 1988 e 2007'),
       xlab = '',
       ylab = 'Participação (%)',
       col = 'springgreen',
       pch = 19)
  
  print(str)
  
  teste2 = lm(as.numeric(productShare)~anos4)
  abline(teste2, col = 'navyblue')
  
  print(summary(teste2))
}


#DEPOIS DE 2008
for(i in 1:nrow(usa_rel)){
  
  str = usa_rel[i, 3]
  
  productShare = c(usa_rel[i, 6:16], chn_rel[i, 6:16], jpn_rel[i, 6:16], deu_rel[i, 6:16])
  anos4 = c(anosD, anosD, anosD, anosD)
  
  plot(y = productShare,
       x = anos4,main = paste(str,': participação nas exportações das quatro maiores economias entre 2008 e 2018'),
       xlab = '',
       ylab = 'Participação (%)',
       col = 'springgreen',
       pch = 19)
  
  print(str)
  
  teste2 = lm(as.numeric(productShare)~anos4)
  abline(teste2, col = 'navyblue')
  
  print(summary(teste2))
}


