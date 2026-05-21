library(ggplot2)

# =========================
# BOXPLOT
# =========================
criar_boxplot <- function(dados, variavel){
  
  ggplot(
    dados,
    aes(x = .data[[variavel]])
  ) +
    
    geom_boxplot(
      fill = "#FAA43A",
      width = 0.3
    ) +
    
    labs(
      title = paste(
        "Boxplot de",
        variavel
      ),
      
      x = variavel
    ) +
    
    theme_minimal()
  
}

# =========================
# HISTOGRAMA
# =========================

criar_histograma <- function(dados, variavel){
  
  ggplot(
    dados,
    aes(x = .data[[variavel]])
  ) +
    
    geom_histogram(
      binwidth = 1,
      
      fill = "#5DA5DA",
      
      color = "white"
    ) +
    
    labs(
      title = paste(
        "Histograma de",
        variavel
      ),
      
      x = variavel,
      
      y = "Frequência"
    ) +
    
    theme_minimal()
  
}

# =========================
# BARRAS NUMÉRICAS
# =========================

criar_barplot_numerico <- function(dados, variavel){
  
  ggplot(
    dados,
    aes(x = factor(.data[[variavel]]))
  ) +
    
    geom_bar(
      fill = "#60BD68"
    ) +
    
    labs(
      title = paste(
        "Distribuição de",
        variavel
      ),
      
      x = variavel,
      
      y = "Frequência"
    ) +
    
    theme_minimal()
  
}

# =========================
# ANÁLISE NUMÉRICA
# =========================

analise_numerica <- function(dados, variavel){
  
  cat("\n====================\n")
  cat("Variável:", variavel)
  cat("\n====================\n\n")
  
  print(
    summary(
      dados[[variavel]]
    )
  )
  
  cat("\n")
  
  cat(
    "Desvio padrão:",
    sd(
      dados[[variavel]],
      na.rm = TRUE
    )
  )
  
  cat("\n")
  
  cat(
    "Variância:",
    var(
      dados[[variavel]],
      na.rm = TRUE
    )
  )
  
}

# =========================
# ANÁLISE CATEGÓRICA
# =========================
analise_categorica <- function(dados, variavel){
  
  frequencia <- dados %>%
    
    count(.data[[variavel]]) %>%
    
    mutate(
      porcentagem = round(
        n / sum(n) * 100,
        2
      )
    ) %>%
    
    arrange(desc(n))
  
  cat("\n====================\n")
  cat("Variável:", variavel)
  cat("\n====================\n\n")
  
  print(frequencia)
  
}

criar_barplot <- function(dados, variavel){
  
  ggplot(
    dados,
    aes(
      y = reorder(
        .data[[variavel]],
        .data[[variavel]],
        function(x) length(x)
      )
    )
  ) +
    
    geom_bar(
      fill = "#5DA5DA"
    ) +
    
    labs(
      title = paste(
        "Frequência de",
        variavel
      ),
      
      x = "Quantidade",
      y = variavel
    ) +
    
    theme_minimal()
  
}



plot_top10 <- function(dados, categoria, titulo = "Top 10") {
  
  top10 <- dados %>%
    group_by(.data[[categoria]]) %>%
    summarise(valor = n()) %>%   # <- conta ocorrências
    slice_max(valor, n = 10) %>%
    arrange(valor)
  
  top10[[categoria]] <- factor(top10[[categoria]], levels = top10[[categoria]])
  
  ggplot(top10, aes(x = .data[[categoria]], y = valor)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    geom_text(aes(label = valor), hjust = -0.1, size = 3.5) +
    labs(title = titulo, x = categoria, y = "Quantidade") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())
}

