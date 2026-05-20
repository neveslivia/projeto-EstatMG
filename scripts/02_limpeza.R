colSums(is.na(acidentes))
any(is.na(acidentes))
sum(is.na(acidentes))
colSums(is.na(acidentes))
acidentes[!complete.cases(acidentes), ]

acidentes$mes <- month(acidentes$data_inversa,
                       label = TRUE,
                       abbr = FALSE)
acidentes$horario_formatado <- 
  format(acidentes$horario, "%H:%M:%S")

acidentes$hora <- as.numeric(format(acidentes$horario, "%H"))

unique(acidentes$km)


acidentes$periodo <- case_when(
  acidentes$hora >= 5 & acidentes$hora < 12 ~ "Manhã",
  acidentes$hora >= 12 & acidentes$hora < 18 ~ "Tarde",
  acidentes$hora >= 18 & acidentes$hora < 24 ~ "Noite",
  TRUE ~ "Madrugada"
)

acidentes$periodo <- factor(
  acidentes$periodo,
  levels = c("Madrugada", "Manhã", "Tarde", "Noite"),
  ordered = TRUE
)

acidentes$dia_semana <- factor(
  acidentes$dia_semana,
  levels = c(
    "segunda",
    "terça",
    "quarta",
    "quinta",
    "sexta",
    "sábado",
    "domingo"
  ),
  ordered = TRUE
)

acidentes$br <- gsub("\\.0", "", acidentes$br)

acidentes$br <- paste0("BR-", acidentes$br)

acidentes$km <- as.numeric(acidentes$km)

acidentes <- acidentes %>%
  relocate(horario,horario_formatado,hora, periodo,fase_dia,mes, .after = data_inversa)
