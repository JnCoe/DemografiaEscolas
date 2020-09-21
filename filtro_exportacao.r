library(data.table)
library(janitor)
library(dplyr)
library(sjmisc)
library(tidyr)

temp <- fread('C:/Users/jonas/Downloads/Temp/teste2.csv')


temp <- temp %>% select(!c(DS_COMPLEMENTO,ID2,`NO_MUNICIPIO.x`,`NO_MUNICIPIO.y`,`Number of Records`,Fundeb, PrecisionDepth, `Unnamed: 0`, X1, X82,`Co Entidade`, Id))

temp <- clean_names(temp)

temp <- temp %>%
  move_columns(c(nd,par,pre), .after = "tp_etapa_ensino")

temp <- temp %>%
  move_columns(total, .after = "bra")

temp <- temp %>%
  move_columns(c(nao_bra_prop,nao_bra_prop_declarados), .after = "total")

temp <- temp %>% rename(afd_fundamental = ind_quali_prof2, afd_ensino_medio = ind_quali_prof, afd_average = media_ind_prof)

temp <- temp %>%
  move_columns(ind, .after = "bra")

write_csv(temp, "mapa_racial.csv")