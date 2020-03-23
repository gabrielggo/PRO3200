calc_5biggest = function(mean_exports){
  collumnNames = c("paises", "medias", "desvpad")
  names(mean_exports) <- collumnNames
  confianca = c()
  mean_exports[order(mean_exports$medias, mean_exports$desvpad, mean_exports$paises, decreasing = c(FALSE, FALSE, FALSE)), ]
  mean_exports
  
  for(i in 1:(length(mean_exports$paises) - 1)){
    c1 = mean_exports$medias[i] - 2*mean_exports$desvpad[i]
    c2 = mean_exports$medias[i + 1] + 2*mean_exports$desvpad[i + 1]
    test = c1 - c2
    x = test > 0
    confianca = append(confianca, x)
  #  if(x){
   #   confianca = append(confianca, FALSE)
    #}else confianca = append(confianca, TRUE)
  }
  if(length(mean_exports) != length(confianca)) {confianca = append(confianca, FALSE)}
  mean_exports$confianca = confianca
  
  return(mean_exports)
}