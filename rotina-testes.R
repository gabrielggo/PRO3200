#FUNÇOES NECESSÁRIAS



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
  
plot(x = anos, y = desvios, type = 'b') #TODO: Melhorar a estética desse gráfico para colocar no relatório
  
teste1 = lm(desvios~anos)
  
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
       main = str)
  
  print(str)
  
  teste2 = lm(as.numeric(productShare)~anos4)
  
  print(summary(teste2))
}




plot(x = c(2018:1989), y = desvios, type = 'b')

cor.test(desvios, c(2018:1989)) #p-valor 2.006

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
       main = str)
  
  print(str)
  
  teste2 = lm(as.numeric(productShare)~anos4)
  
  print(summary(teste2))
}


#DEPOIS DE 2008
for(i in 1:nrow(usa_rel)){
  
  str = usa_rel[i, 3]
  
  productShare = c(usa_rel[i, 6:36], chn_rel[i, 6:16], jpn_rel[i, 6:16], deu_rel[i, 6:16])
  anos4 = c(anosD, anosD, anosD, anosD)
  
  plot(y = productShare,
       x = anos4,
       main = str)
  
  print(str)
  
  teste2 = lm(as.numeric(productShare)~anos4)
  
  print(summary(teste2))
}


