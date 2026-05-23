library(ggplot2)
library(dplyr)


cor_principal <- "#355070"
cor_secundaria <- "#6D597A"
cor_destaque <- "#B56576"
cor_outlier <- "#C94C4C"

paleta_bivariada <- c(
  "#355070",
  "#4F5D75",
  "#6D597A",
  "#8E6C88",
  "#B56576",
  "#D17A88",
  "#E56B6F",
  "#EAAC8B",
  "#7F5539",
  "#9C6644",
  "#B08968",
  "#D4A373"
)



arrumar_nome <- function(texto){
  
  texto <- gsub("_", " ", texto)
  
  texto <- tools::toTitleCase(texto)
  
  return(texto)
  
}



tema_projeto <- function(){
  
  theme_classic() +
    
    theme(
      
      plot.title = element_text(
        size = 14,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 12)
      ),
      
      axis.title = element_text(
        size = 11
      ),
      
      axis.text = element_text(
        size = 10,
        color = "black"
      ),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold"
      ),
      
      panel.border = element_blank(),
      
      plot.margin = margin(
        15, 15, 15, 15
      )
      
    )
  
}


criar_boxplot <- function(dados, variavel){
  
  nome <- arrumar_nome(variavel)
  
  ggplot(
    dados,
    aes(x = "", y = .data[[variavel]])
  ) +
    
    geom_boxplot(
      fill = cor_principal,
      width = 0.3,
      outlier.color = cor_outlier,
      alpha = 0.85
    ) +
    
    labs(
      title = paste("Boxplot de", nome),
      x = NULL,
      y = nome
    ) +
    
    tema_projeto()
  
}


criar_histograma <- function(dados, variavel){
  
  nome <- arrumar_nome(variavel)
  
  ggplot(
    dados,
    aes(x = .data[[variavel]])
  ) +
    
    geom_histogram(
      binwidth = 1,
      fill = cor_principal,
      color = "white"
    ) +
    
    labs(
      title = paste("Histograma de", nome),
      x = nome,
      y = "Frequência"
    ) +
    
    tema_projeto()
  
}



criar_barplot_numerico <- function(dados, variavel){
  
  nome <- arrumar_nome(variavel)
  
  ggplot(
    dados,
    aes(x = factor(.data[[variavel]]))
  ) +
    
    geom_bar(
      fill = cor_principal,
      width = 0.7
    ) +
    
    labs(
      title = paste("Distribuição de", nome),
      x = nome,
      y = "Frequência"
    ) +
    
    tema_projeto()
  
}


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
  
  nome <- arrumar_nome(variavel)
  
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
      fill = cor_principal
    ) +
    
    labs(
      title = paste("Frequência de", nome),
      x = "Quantidade",
      y = nome
    ) +
    
    tema_projeto()
  
}



plot_top10 <- function(dados, categoria, titulo = "Top 10") {
  
  top10 <- dados %>%
    
    count(.data[[categoria]]) %>%
    
    slice_max(n, n = 10) %>%
    
    arrange(n)
  
  ggplot(
    top10,
    aes(
      y = reorder(.data[[categoria]], n),
      x = n
    )
  ) +
    
    geom_col(
      fill = cor_principal,
      width = 0.7
    ) +
    
    geom_text(
      aes(label = n),
      hjust = -0.15,
      size = 3.5
    ) +
    
    labs(
      title = titulo,
      x = "Quantidade",
      y = NULL
    ) +
    
    tema_projeto()
  
}





criar_boxplot_bivariado <- function(dados, categ, numerica){
  
  nome1 <- arrumar_nome(categ)
  nome2 <- arrumar_nome(numerica)
  
  ggplot(
    dados,
    aes(
      x = .data[[categ]],
      y = .data[[numerica]]
    )
  ) +
    
    geom_boxplot(
      fill = cor_principal,
      alpha = 0.85,
      outlier.color = cor_outlier
    ) +
    
    labs(
      title = paste(nome2, "por", nome1),
      x = nome1,
      y = nome2
    ) +
    
    tema_projeto()
  
}


criar_grafico_temporal <- function(dados, variavel){
  
  nome <- arrumar_nome(variavel)
  
  dados %>%
    
    count(.data[[variavel]]) %>%
    
    ggplot(
      aes(
        x = .data[[variavel]],
        y = n,
        group = 1
      )
    ) +
    
    geom_line(
      color = cor_principal,
      linewidth = 1
    ) +
    
    geom_point(
      color = cor_principal,
      size = 3
    ) +
    
    labs(
      title = paste("Acidentes por", nome),
      x = nome,
      y = "Quantidade"
    ) +
    
    tema_projeto() +
    
    theme(
      axis.text.x = element_text(
        angle = 30,
        hjust = 1
      )
    )
  
}



criar_barplot_horizontal <- function(dados, variavel){
  
  nome <- arrumar_nome(variavel)
  
  dados %>%
    
    count(.data[[variavel]]) %>%
    
    ggplot(
      aes(
        y = reorder(.data[[variavel]], n),
        x = n
      )
    ) +
    
    geom_col(
      fill = cor_principal,
      width = 0.7
    ) +
    
    labs(
      title = paste("Distribuição de", nome),
      x = "Quantidade",
      y = NULL
    ) +
    
    tema_projeto()
  
}



acidentes_top <- acidentes %>%
  
  filter(
    tipo_acidente %in% c(
      "Saída de leito carroçável",
      "Colisão traseira",
      "Tombamento",
      "Colisão lateral",
      "Capotamento"
    )
  )



paleta_projeto <- c(
  "#355070",
  "#6D597A",
  "#B56576",
  "#E56B6F",
  "#EAAC8B",
  "#5E548E",
  "#9F86C0",
  "#7B2CBF",
  "#C77DFF",
  "#F4A261",
  "#2A9D8F",
  "#264653",
  "#8AB17D",
  "#A44A3F",
  "#577590"
)

criar_bivariado_empilhado <- function(dados, var1, var2){
  
  nome1 <- arrumar_nome(var1)
  nome2 <- arrumar_nome(var2)
  
  ggplot(
    dados,
    aes(
      x = .data[[var1]],
      fill = .data[[var2]]
    )
  ) +
    
    geom_bar(
      position = "fill",
      width = 0.75
    ) +
    
    scale_y_continuous(
      labels = scales::percent
    ) +
    
    scale_fill_manual(
      values = paleta_projeto
    ) +
    
    guides(
      fill = guide_legend(
        nrow = 4
      )
    ) +
    
    labs(
      title = paste(nome1, "por", nome2),
      x = nome1,
      y = "Proporção",
      fill = nome2
    ) +
    
    tema_projeto() +
    
    theme(
      
      axis.text.x = element_text(
        angle = 20,
        hjust = 1
      ),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        size = 10,
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 8
      ),
      
      legend.box = "vertical"
      
    )
  
}
criar_bivariado_cat <- function(dados, var1, var2){
  
  nome1 <- arrumar_nome(var1)
  nome2 <- arrumar_nome(var2)
  
  ggplot(
    dados,
    aes(
      x = .data[[var1]],
      fill = .data[[var2]]
    )
  ) +
    
    geom_bar(
      position = "dodge",
      width = 0.7
    ) +
    
    scale_fill_manual(
      values = paleta_projeto
    ) +
    
    guides(
      fill = guide_legend(
        nrow = 4
      )
    ) +
    
    labs(
      title = paste(nome1, "por", nome2),
      x = nome1,
      y = "Quantidade",
      fill = nome2
    ) +
    
    tema_projeto() +
    
    theme(
      
      axis.text.x = element_text(
        angle = 20,
        hjust = 1
      ),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        size = 10,
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 8
      ),
      
      legend.box = "vertical"
      
    )
  
}