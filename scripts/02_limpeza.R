

colSums(is.na(acidentes))

any(is.na(acidentes))

sum(is.na(acidentes))

sum(is.na(acidentes$km))





acidentes$mes <- month(
  acidentes$data_inversa,
  label = TRUE,
  abbr = FALSE
)

acidentes$horario_formatado <- format(
  acidentes$horario,
  "%H:%M:%S"
)

acidentes$hora <- hour(acidentes$horario)

acidentes$periodo <- case_when(
  acidentes$hora >= 5 & acidentes$hora < 12 ~ "Manhã",
  acidentes$hora >= 12 & acidentes$hora < 18 ~ "Tarde",
  acidentes$hora >= 18 & acidentes$hora < 24 ~ "Noite",
  TRUE ~ "Madrugada"
)




acidentes$periodo <- factor(
  acidentes$periodo,
  levels = c(
    "Madrugada",
    "Manhã",
    "Tarde",
    "Noite"
  ),
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

acidentes$br <- gsub("BR-", "", acidentes$br)

acidentes$br <- trimws(acidentes$br)

acidentes$br <- paste0("BR-", acidentes$br)

acidentes$br[acidentes$br == "BR-NA"] <- NA




acidentes$km <- trimws(acidentes$km)

acidentes$km[acidentes$km == "NA"] <- NA

acidentes$km <- as.numeric(acidentes$km)

sum(is.na(acidentes$km))


acidentes <- acidentes %>%
  relocate(
    horario,
    horario_formatado,
    hora,
    periodo,
    fase_dia,
    mes,
    .after = data_inversa
  )


verificacao_feridos <- all(
  acidentes$feridos ==
    acidentes$feridos_leves +
    acidentes$feridos_graves
)



glimpse(acidentes)

count(acidentes, tipo_acidente, sort = TRUE)

top10_acidentes <- acidentes %>%
  
  count(tipo_acidente, sort = TRUE) %>%
  
  slice_head(n = 10)



acidentes_top10 <- acidentes %>%
  
  filter(
    tipo_acidente %in% top10_acidentes$tipo_acidente
  )