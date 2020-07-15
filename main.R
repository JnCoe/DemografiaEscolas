library(dplyr)
library(readr)
library(janitor)

# Para filtrar o CSV de microdados por município, rode no shell linux:
# awk -F "|" '{if ($87 == CODIGODOMUNICIPIO) {print}}' input.csv > output.csv
# Substitua os termos pelo codigo do município desejado bem como pelos arquivos condizentes

matri_se <- read_delim("Dados\\MATRICULA_SUDESTE.CSV", delim="|", n_max=100)

matri_sp <- read_delim("Dados\\sp_capital.csv", delim="|", col_names=FALSE)

equi <- data.frame(colnames(matri_se))


# TP_COR_RACA = X9
# "0 - Não declarada
# 1 - Branca
# 2 - Preta
# 3 - Parda
# 4 - Amarela
# 5 - Indígena"


# CO_ENTIDADE = X82


escolas_sp <- matri_sp %>%
  tabyl(X82, X9)

oldnames = c("0","1","2","3","4","5")
newnames = c("nd","bra","pre","par","ama","ind")

escolas_sp <- escolas_sp %>% rename_at(vars(oldnames), ~ newnames)

escolas_sp <- escolas_sp %>%
  mutate(total = nd+bra+pre+par+ama+ind)

escolas_sp <- escolas_sp %>%
  mutate(nao_bra_prop = (1-(bra/total))*100)
