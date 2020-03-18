###########################
# Olá grupo (para quem tiver a curiosidade de ter aberto)
# Esse script são algumas experiências para ter ideia de como a gente vai trabalhar com os dados
# Focando na indonesia por enquanto, depois expando a análise pra outros países

#Importar dados de exportação (mudar endereço para funcionar no seu pc)
indonesia_trade <- read.csv(
  file = "F:/Google Drive/Compartilhado/Projeto/dados-e-scripts/wits_en_trade_summary_allcountries_allyears/en_IDN_AllYears_WITS_Trade_Summary.CSV",
  header = T, sep = ',', dec = '.', stringsAsFactors = FALSE
  )

#Importar dados de taxa de câmbio (mudar endereço para funcionar no seu pc)
indonesia_exchange <- read.csv(
  file = "F:/Google Drive/Compartilhado/Projeto/dados-e-scripts/API_PA.NUS.FCRF_DS2_en_csv_v2_823342/API_PA.NUS.FCRF_DS2_en_csv_v2_823342.csv",
  header = F, sep = ",", dec = '.', stringsAsFactors = FALSE
)

#--------------------------
#Primeio passo: limpar dados

#Limpar os dados de comércio
myvars <- names(indonesia_trade) %in% c("Reporter", "Indicator.Type")
indonesia_exports <- indonesia_trade[which(indonesia_trade$Indicator=='Export(US$ Mil)'| indonesia_trade$Indicator == "Exports (in US$ Mil)"), !myvars]
library(reshape2)
indonesia_trade_long <- melt(indonesia_exports, variable.name = "Year")
indonesia_trade_long$Year <- as.numeric(format(as.Date(indonesia_trade_long$Year, format = "X%Y"),"%Y"))

#Plotar a série histórica de exportações de todos os produtos
plot(
  x = indonesia_trade_long[which(indonesia_trade_long$Product.categories == "All Products"),]$Year,
  y = indonesia_trade_long[which(indonesia_trade_long$Product.categories == "All Products"),]$value,
  type = "l"
)

#limpar dados de taxa de câmbio
colnames(indonesia_exchange) <- indonesia_exchange[3,]
indonesia_exchange <- indonesia_exchange[-c(1,2,3),]
indonesia_exchange_long <- melt(indonesia_exchange,id.vars = names(indonesia_exchange[1:4]), variable.name = "Year")
indonesia_exchange_long$Year <- as.numeric(format(as.Date(indonesia_exchange_long$Year, format = "%Y"),"%Y"))

#plotar série histórica da taxa câmbio da indonesia
plot(
  x = indonesia_exchange_long[which(indonesia_exchange_long$`Country Name` == "Indonesia"),]$Year,
  y = indonesia_exchange_long[which(indonesia_exchange_long$`Country Name` == "Indonesia"),]$value,
  type = "b"
)

#--------------------------
#Segundo passo: calcular variação anual das paradinha
calcula_variacao_anual <- function(indicator_series){
  for( i in 1:nrow(indicator_series)){
    print(i)
    for(j in 1:nrow(indicator_series)){
      if(
        (indicator_series[i,"Year"] == (indicator_series[j, "Year"] + 1))
        &(indicator_series[i,"Product.categories"] == indicator_series[j,"Product.categories"])
        &(indicator_series[i,"Indicator"] == indicator_series[j,"Indicator"])
        ){
        #print(indicator_series[i,])
        #print(indicator_series[j,])
        if(!is.na(indicator_series[j,]["value"])){
          ind_name <- paste("Var", indicator_series[i, "Indicator"])
          #print("Encontrou antecessor!")
          novo_valor <- indicator_series[i, "value"]/indicator_series[j, "value"]
          nova_linha <- list(indicator_series[i,"Partner"], 
                        indicator_series[i,"Product.categories"], 
                        ind_name,
                        indicator_series[i, "Year"],
                        novo_valor
                        )
          indicator_series <- rbind(
            indicator_series, nova_linha)

        }
      }
    }
  }
  returnValue(indicator_series)
}

#plotar série histórica da variação da taxa de câmbio
plot(
  x = indonesia_trade_long[which(
    (indonesia_trade_long$Product.categories == "All Products")
    &(indonesia_trade_long$Indicator == "Var Exports (in US$ Mil)")
    ),]$Year,
  y = indonesia_trade_long[which(
    (indonesia_trade_long$Product.categories == "All Products")
    &(indonesia_trade_long$Indicator == "Var Exports (in US$ Mil)")
  ),]$value - 1,
  type = "l"
)


