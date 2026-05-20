criar_boxplot <- function(dados, variavel){
  
  boxplot(
    dados[[variavel]],
    main = paste("Boxplot de", variavel),
    ylab = variavel,
    col = "lightblue",
    border = "darkblue",
    horizontal = TRUE
  )
  
}

criar_histograma <- function(dados, variavel){
  
  hist(
    dados[[variavel]],
    main = paste("Histograma de", variavel),
    xlab = variavel,
    col = "lightgreen",
    border = "darkgreen"
  )
  
}
