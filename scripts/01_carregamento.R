library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)


acidentes <- read_excel("../dados/acidentes_2020.xlsx")

acidentes <- clean_names(acidentes)