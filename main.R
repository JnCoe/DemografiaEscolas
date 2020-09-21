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

escolas_sp <- escolas_sp %>%
  mutate(nao_bra_prop_declarados = (1-(bra/(bra+pre+par+ama+ind)))*100)

escolas_sp_filtradas <- escolas_sp %>%
  filter(total>40)

write_csv(escolas_sp_filtradas, "escolas_sp_filtradas.csv")


#AFD

library(readxl)
afd <- read_xlsx("Dados/AFD_ESCOLAS_2018.xlsx", skip = 10)

library(dplyr)

afd$AFD_FUN1 <- as.numeric(as.character(afd$AFD_FUN1))
afd$AFD_FUN2 <- as.numeric(as.character(afd$AFD_FUN2))
afd$AFD_FUN3 <- as.numeric(as.character(afd$AFD_FUN3))
afd$AFD_FUN4 <- as.numeric(as.character(afd$AFD_FUN4))
afd$AFD_FUN5 <- as.numeric(as.character(afd$AFD_FUN5))


afd <- afd %>% mutate(ind_quali_prof = ((AFD_FUN1 * 5) + (AFD_FUN2 * 4) + (AFD_FUN3 * 3) + (AFD_FUN4 * 2) + (AFD_FUN5 * 1))/500)


afd$AFD_MED1 <- as.numeric(as.character(afd$AFD_MED1))
afd$AFD_MED2 <- as.numeric(as.character(afd$AFD_MED2))
afd$AFD_MED3 <- as.numeric(as.character(afd$AFD_MED3))
afd$AFD_MED4 <- as.numeric(as.character(afd$AFD_MED4))
afd$AFD_MED5 <- as.numeric(as.character(afd$AFD_MED5))

afd <- afd %>% mutate(ind_quali_prof2 = ((AFD_MED1 * 5) + (AFD_MED2 * 4) + (AFD_MED3 * 3) + (AFD_MED4 * 2) + (AFD_MED5 * 1))/500)

afd$media_ind_prof <- rowMeans(afd[,c('ind_quali_prof', 'ind_quali_prof2')], na.rm=TRUE)

afd_filtrada <- afd %>% select(PK_COD_ENTIDADE, ind_quali_prof, ind_quali_prof2, media_ind_prof)

afd_filtrada <- afd_filtrada %>% rename(co_entidade = PK_COD_ENTIDADE)

final_sem_na <- left_join(final_sem_na,afd_filtrada, by='co_entidade')





# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(scales)


# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)



trace(grDevices:::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

# sample size
sample_size = final_sem_na %>% group_by(co_municipio) %>% summarize(num=n())

# Plot
final_sem_na %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(co_municipio, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=nao_bra_prop_declarados, fill=co_municipio)) +
  scale_y_continuous() +
  geom_violin(width=1.4, color="#636466", fill="#f9a521", draw_quantiles = c(0.25, 0.75))  +
  geom_boxplot(width=0.1, color="black", alpha=0.7, fill="#636466") +
  scale_fill_viridis() +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
    axis.title.x = element_text(angle=0, vjust = 0.5),
    axis.title.y = element_text(angle=0, vjust = 0.5)
  ) +
  xlab("") +
 ylab("Proporção não-brancos")

summary(final_sem_na$salario_medio)



final_sem_na$co_entidade_factor <- factor(final_sem_na$co_entidade)


final_sem_na_bra_filtrado <- final_sem_na %>% select(co_entidade_factor, nao_bra_prop, nao_bra_prop_declarados )

library(tidyr)
final_long_bra <- gather(final_sem_na_bra_filtrado, tipo, percentagem, nao_bra_prop:nao_bra_prop_declarados, factor_key=TRUE)

# sample size
sample_size = final_long_bra %>% group_by(tipo) %>% summarize(num=n())

# Plot
final_long_bra %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(tipo)) %>%
  ggplot( aes(x=myaxis, y=percentagem, fill=tipo)) +
  scale_y_continuous(labels=scales::percent_format(scale=1), limits = c(0,100) ) +
  geom_violin(width=1, color="#636466", fill="#f9a521", draw_quantiles = c(0.25, 0.75), position = position_dodge(width=0.7))  +
  geom_boxplot(width=0.1, color="black", alpha=0.7, fill="#636466") +
  scale_fill_viridis() +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
    axis.title.x = element_text(angle=0, vjust = 0.5),
    axis.title.y = element_text(angle=0, vjust = 0.5)
  ) +
  xlab("") +
  ylab("Proporção não-brancos")

summary(final_sem_na$salario_medio)

summary(final_long_bra, by=tipo)



final_long_bra %>%
  group_by(tipo) %>%
  summarize(mean = mean(percentagem), sd = sd(percentagem))


final_long_bra %>%
  filter(tipo=="nao_bra_prop") %>%
  summarize()

summarize(final_long_bra, mean=mean(tipo))



# sample size
sample_size = final_sem_na %>% group_by(co_municipio) %>% summarize(num=n())

# Plot
final_sem_na %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(co_municipio)) %>%
  ggplot( aes(x=myaxis, y=media_ind_prof, fill=co_municipio)) +
  scale_y_continuous() +
  geom_violin(width=1, color="#636466", fill="#f9a521", draw_quantiles = c(0.25, 0.75), position = position_dodge(width=0.7))  +
  geom_boxplot(width=0.1, color="black", alpha=0.7, fill="#636466") +
  scale_fill_viridis() +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
    axis.title.x = element_text(angle=0, vjust = 0.5),
    axis.title.y = element_text(angle=0, vjust = 0.5)
  ) +
  xlab("") +
  ylab("Índice AFD")

summary(final_sem_na$media_ind_prof)


model1 <- lm(data= final_sem_na, salario_medio ~ nao_bra_prop_declarados)
library(outreg)

library("xlsx")


write.xlsx(outreg(model1), "model1.xlsx")

model2 <- lm(data= final_sem_na, salario_medio ~ nao_bra_prop_declarados + media_ind_prof)
write.xlsx(outreg(model2), "model2.xlsx")


model3 <- lm(data=final_sem_na, dist ~ salario_medio)
model4 <- lm(data=final_sem_na, dist ~ nao_bra_prop_declarados)

write.xlsx(outreg(model3), "model3.xlsx")
write.xlsx(outreg(model4), "model4.xlsx")


final_sem_na$ppi <-final_sem_na$ppi*100

modelo_final1 <- lm(data=final_sem_na, salario_medio ~ dist)
modelo_final2 <- lm(data=final_sem_na, salario_medio ~ ppi)                     

summary(modelo_final2)


library(ggplot2)
library(hrbrthemes)
p2 <- ggplot(data=final_sem_na, aes(x=ppi, y=salario_medio)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  scale_y_continuous(labels=scales::dollar_format(prefix = "R$")) +
  labs(x="Porcentagem de alunos PPI", y="Salário Médio") +
  theme_ipsum()

p2



p3 <- final_sem_na %>% ggplot(aes(x=dist2, y=salario_medio)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum() +
  scale_y_continuous(labels=scales::dollar_format(prefix = "R$")) +
  labs(x="Distância euclidiana ao Centro", y="Salário Médio")
p3

library(outreg)
x <- outreg(modelo_final1)
x
y <- outreg(modelo_final2)
y

summary(modelo_final2)

final_sem_na$dist2 <-final_sem_na$dist*10
modelo_final3 <- lm(data=final_sem_na, salario_medio ~ dist2)

z <- outreg(modelo_final3)
z
