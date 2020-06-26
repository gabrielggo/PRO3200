files <- list.files('dados_1afase', full.names = TRUE)
a = c('wits_en_trade_summary_allcountries_allyears/en_USA_AllYears_WITS_Trade_Summary.CSV',
      'wits_en_trade_summary_allcountries_allyears/en_CHN_AllYears_WITS_Trade_Summary.CSV',
      'wits_en_trade_summary_allcountries_allyears/en_JPN_AllYears_WITS_Trade_Summary.CSV', 
      'wits_en_trade_summary_allcountries_allyears/en_DEU_AllYears_WITS_Trade_Summary.CSV', 
      'wits_en_trade_summary_allcountries_allyears/en_FRA_AllYears_WITS_Trade_Summary.CSV'
      )


#Ajustar dados dos países

pais = read.csv('wits_en_trade_summary_allcountries_allyears/en_CHN_AllYears_WITS_Trade_Summary.CSV',
                header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)

exp_absolutas = getExportacoesPaises(pais)
exp_relativas = getProductshare(pais)

exp_absolutas = deflator(exp_absolutas) #Deflaciona os dados absolutos

exp_absolutas = agrega_menores_setores(exp_absolutas)
exp_relativas = agrega_menores_setores(exp_relativas)

exp_crescimento = crescimentoAnual(exp_absolutas)




#Desvio padrão do crescimento dos setores ao longo do ano
desvios <- c()

for(i in 6:35){
  desvios[i - 5] <- sd(exp_relativas[,i])
}

plot(x = c(2018:1989), y = desvios)

cor.test(desvios, c(2018:1989)) #p-valor 2.006


