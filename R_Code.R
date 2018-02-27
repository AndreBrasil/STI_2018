### STI 2018
### André Brasil

##############################
# Installing packages
##############################

install.packages("plyr")
install.packages("readxl")
install.packages("lubridate")
install.packages("reshape2")
install.packages("ggthemes")
install.packages("tidyverse")
install.packages("qdapTools")

#---- Updating tidyverse

tidyverse_update()


##############################
# Loading packages
##############################

library(plyr)
library(readxl)
library(lubridate)
library(reshape2)
library(ggthemes)
library(tidyverse)
library(qdapTools)

###################################
# Building necessary functions
###################################

#---- function for renaming and cleaning column names

clean_names <- function(dat, case = c("snake", "small_camel", "big_camel", "screaming_snake", 
                                      "parsed", "mixed", "lower_upper", "upper_lower",
                                      "all_caps", "lower_camel", "upper_camel", "old_janitor")) {
  case <- match.arg(case)
  if (case == "old_janitor"){
    return(old_clean_names(dat))  
  }
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("'", "", .) %>% 
    gsub("\"", "", .) %>% 
    gsub("%", ".perc", .) %>% 
    gsub("#", ".n_", .) %>%
    gsub("^[[:space:][:punct:]]+", "", .) %>% 
    make.names(.) %>%
    snakecase::to_any_case(case = case, preprocess = "\\.", 
                           replace_special_characters = c("Latin-ASCII"))
  dupe_count <- vapply(1:length(new_names), function(i) { 
    sum(new_names[i] == new_names[1:i]) }, integer(1))
  
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  stats::setNames(dat, new_names)
}

########################################
# Importing, binding and cleaning data
########################################

file.list.tmp <- dir(path = "Dados/PlanilhasAreas/", pattern = "*.xlsx", recursive = TRUE, full.names = TRUE)

#---- Scientific production list

production.list.tmp <- lapply(file.list.tmp, read_xlsx, sheet = 7, col_names = TRUE, na = "", skip = 5)
  production.list.tmp[[2]] <- production.list.tmp[[2]][,-c(18,22:24,59)] 
  production.list.tmp[[27]] <- production.list.tmp[[27]][,-c(20:21,29)] 
  production.list.tmp[[13]] <- production.list.tmp[[13]][,-c(21)] 
  for (i.tmp in c(8,11,12,20,26,33,40)) {production.list.tmp[[i.tmp]] <- production.list.tmp[[i.tmp]][,-c(18)]}
  
  for (i.tmp in 1:49) {
    production.list.tmp[[i.tmp]] <- production.list.tmp[[i.tmp]] %>%
      mutate_at(.vars = vars(13,15),
                .funs = funs(as.numeric))
  }
  
  data.production.tmp <- bind_rows(production.list.tmp)
  
#---- scientific production per postgraduate program
  
  production.program.tmp <- lapply(file.list.tmp, read_xlsx, sheet = 8, col_names = TRUE, na = "", skip = 5)
  production.program.tmp[[33]] <- production.program.tmp[[33]][,-c(56:69)] 
  production.program.tmp[[38]] <- production.program.tmp[[38]][,-c(56:59)] 
  
  for (i.tmp in 1:49) {
    production.program.tmp[[i.tmp]] <- production.program.tmp[[i.tmp]] %>%
      mutate_at(.vars = vars(13),
                .funs = funs(as.numeric))
  }
  
  data.program.tmp <- bind_rows(production.program.tmp)

#---- Additional program data

evaluation.areas.tmp <- read_excel("Dados/AreasAvaliacao.xlsx", col_names = TRUE)
institutions.tmp <- read_excel("Dados/dados_ies.xlsx", col_names = TRUE)
programs.tmp <- read_excel("Dados/DadosPPGs.xlsx", col_names = TRUE)
geography.tmp <- read_excel("Dados/municipios_ibge.xlsx", col_names = TRUE)

join1.tmp <- left_join(programs.tmp, institutions.tmp, by = c("IES_CD" = "IesCd"), suffix = c("","_2"))
join2.tmp <- left_join(join1.tmp, evaluation.areas.tmp, by = c("Área de Avaliação" = "Área de Avaliação"), suffix = c("","_2"))
geography.tmp$MunicípioCDCompleto <- as.numeric(geography.tmp$MunicípioCDCompleto)
join3.tmp <- left_join(join2.tmp, geography.tmp, by = c("MunicipioCD" = "MunicípioCDCompleto"), suffix = c("","_2"))

data.additional.tmp <- join3.tmp[,c(1,7:9,22,28,30,35,36,45,50)]

#---- clean data

data.production.tmp <- clean_names(data.production.tmp, "upper_camel")
data.program.tmp <- clean_names(data.program.tmp, "upper_camel")
data.researcher.tmp <- clean_names(data.researcher.tmp, "upper_camel")
data.additional.tmp <- clean_names(data.additional.tmp, "upper_camel")

#---- adjusting formats

data.production.tmp$Tipo <- ifelse(data.production.tmp$Tipo == "ARTISTICA", "ARTÍSTICA",
                               ifelse(data.production.tmp$Tipo == "BIBLIOGRAFICA", "BIBLIOGRÁFICA",
                                      ifelse(data.production.tmp$Tipo == "TECNICA", "TÉCNICA", 
                                             data.production.tmp$Tipo)))

data.production.tmp <- data.production.tmp %>%
  mutate_at(.vars = vars(1, 5:6, 8:11, 13:14, 16, 18:20, 24, 41:51),
            .funs = funs(as.factor))
data.production <- data.production.tmp[which(data.production.tmp$CodPpg %in% data.additional.tmp$CodigoDoPrograma), -c(52:55)]
data.production.tableau <- data.production[,c(1:2, 11, 14, 17:20, 24)]

data.production.tableau %>% 
  group_by(CodPpg) %>% 
  tally()

data.program.tmp <- data.program.tmp %>%
  mutate_at(.vars = vars(1, 5:6, 8:11, 13:14, 16, 18),
            .funs = funs(as.factor))
data.program <- data.program.tmp[,-c(121:124)] 

data.researcher.tmp <- data.researcher.tmp %>%
  mutate_at(.vars = vars(1, 5:6, 8:11, 13:14, 16, 18),
            .funs = funs(as.factor))
data.researcher <- data.researcher.tmp[,-c(113:130)] 

data.additional <- data.additional.tmp %>%
  mutate_at(.vars = vars(2:9),
            .funs = funs(as.factor))

rm(list = ls(pattern = ".tmp")) #Clean environment

#---- create data.frame for Tableau Exploration

data.tableau <- left_join(data.additional, data.program, by = c("CodigoDoPrograma" = "CodPpg"), suffix = c("","_2"))
data.tableau <- data.tableau[,c(1, 13, 3:4, 9, 8, 2, 6, 7, 5, 10, 19:20, 11:12, 27:31, 33:47, 51:88, 98, 108, 118, 128:130)]
data.tableau <- plyr::rename(data.tableau, c("Total" = "Docentes",
                                             "IesSiglaAgrupada" = "IES",
                                             "TotalDistinto" = "Discentes",
                                             "Total1" = "Artigos",
                                             "Outros" = "OutrosLivros",
                                             "Total2" = "Livros",
                                             "TrabalhoCompleto1" = "AnaisTbCompleto",
                                             "ResumoExpandido" = "AnaisResumoExp",
                                             "Resumo1" = "AnaisResumo",
                                             "Total3" = "Anais",
                                             "Outro" = "OutroTecnico",
                                             "TotalTecnica" = "Tecnica",
                                             "Total4" = "ArtesCenicas",
                                             "Total5" = "ArtesVisuais",
                                             "Total6" = "Musica",
                                             "Total7" = "OutrosArtistica",
                                             "TotalDeArtistica" = "Artistica",
                                             "TotalGeralDeProducoes" = "Producao"))

names(data.tableau)
summary(data.tableau)
data.production.tableau %>% 
  group_by(CodPpg) %>% 
  tally()

summary(data.production)
summary(data.production.tableau)
summary(data.program)
summary(data.researcher)
summary(data.additional)
summary(data.tableau)


############################
# Calculating fields
############################

data.tableau <- data.tableau %>%
  group_by(CodigoDoPrograma) %>%
  mutate(Registros = n())%>%
  mutate(MediaBibliografica = mean(TotalBibliografica)) %>%
  mutate(MedianaBibliografica = median(TotalBibliografica)) %>%
  mutate(MediaArtisticaTecnica = mean(Tecnica+Artistica)) %>%
  mutate(MedianaArtisticaTecnica = median(Tecnica+Artistica))%>%
  mutate(RazaoBiblioTecnica = mean(mean(TotalBibliografica)/mean(Tecnica+Artistica)))


############################
# Data exploration
############################

levels(data.production$Tipo)
levels(data.production$Subtipo)
levels(data.tableau$Nivel)
levels(data.tableau$Modalidade)

data.production.tableau$Subtipo %>%
  table(data.production.tableau$Tipo, data.production.tableau$Modalidade) %>%
  round(2)

data.production.tableau$Subtipo %>%
  table(data.production.tableau$Tipo, data.production.tableau$Modalidade) %>%
  prop.table() %>% {. * 1000} %>% 
  round(2)

ggplot(data.production.tableau, aes(x = data.production.tableau$Subtipo, fill = data.production.tableau$Modalidade)) +
  geom_bar() + coord_flip() +
  facet_grid(data.production.tableau$Tipo ~ . , scale = "free_y", space = "free_y") +
  theme(legend.position = "bottom")

data.tableau %>%
  group_by(AreaDeAvaliacao)%>%
  ggplot(., aes(x = factor(Modalidade), y = RazaoBiblioTecnica))+
  geom_boxplot()+
  scale_y_log10()

data.tableau %>%
  group_by(AreaDeAvaliacao)%>%
  ggplot(., aes(x = factor(Modalidade), y = RazaoBiblioTecnica))+
  geom_boxplot()+
  facet_wrap(~GrandeArea, ncol = 9)+
  scale_y_log10()

data.production %>% 
  filter(Nivel %in% c("MESTRADO", "MESTRADO PROFISSIONAL")) %>%
  group_by(Modalidade, CodPpg, Tipo) %>%
  summarise(total = n(), média = round(n()/n_distinct(AnoBase), 0))%>%
  ggplot(aes(y = média, x = Modalidade))+geom_boxplot()+facet_grid(~Tipo)

data.production %>% 
  filter(Nivel %in% c("MESTRADO", "MESTRADO PROFISSIONAL")) %>%
  group_by(Modalidade, CodPpg, Tipo) %>%
  filter(n() >= 2000)%>%
  summarise(total = n(), média = round(n()/n_distinct(AnoBase), 0))

data.production %>%
  group_by(Nivel)%>%
  summarise(média = round((n()/n_distinct(CodPpg))/n_distinct(AnoBase), 0))

data.production %>%
  group_by(CodPpg)%>%
  filter(CodPpg %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1"))%>%
  summarise(total = n(), média = round(n()/n_distinct(AnoBase), 0))

data.tableau %>%
  group_by(GrandeArea, AreaDeAvaliacao)%>%
  filter(Nivel %in% c("Mestrado", "Mestrado Profissional"))%>%
  filter(!(CodigoDoPrograma %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1")))%>%
  summarise(Academic = n_distinct(CodigoDoPrograma[grep("ACADÊMICO", Modalidade)]), Professional = n_distinct(CodigoDoPrograma[grep("PROFISSIONAL", Modalidade)])) -> research.courses
View(research.courses)

ggplot(data.production, aes(x = factor(Tipo, levels = c("BIBLIOGRÁFICA", "TÉCNICA", "ARTÍSTICA"))))+
  geom_bar(fill = "steelblue") +
  facet_wrap(~Modalidade)

ggplot(data.production, aes(x= factor(Tipo, levels = c("BIBLIOGRÁFICA", "TÉCNICA", "ARTÍSTICA")), group=Modalidade)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = ..count.., y= ..prop..), stat= "count", vjust = -.5) +
  labs(y = "Porcentagem", x="Tipo de produção") +
  facet_grid(. ~ Modalidade, scale = "free_y", space = "free_y") +
  theme(legend.position = "none")

data.tableau %>%
  filter(Nivel %in% c("Mestrado", "Mestrado Profissional"))%>%
  filter(AnoBase == 2016) %>%
  filter(!(CodigoDoPrograma %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1")))%>%
ggplot(., aes(MediaBibliografica, MediaArtisticaTecnica, col = Modalidade)) +
  geom_point(alpha = 0.3) + geom_smooth(method = "lm", fullrange = TRUE) + 
  scale_x_log10() + scale_y_log10() + theme_bw()

data.tableau %>%
  filter(Nivel %in% c("Mestrado", "Mestrado Profissional"))%>%
  filter(AnoBase == 2016) %>%
  filter(!(CodigoDoPrograma %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1")))%>%
  lm(MediaArtisticaTecnica ~ MediaBibliografica + Modalidade, data = .)%>%
  summary()

data.tableau %>%
  filter(Nivel %in% c("Mestrado", "Mestrado Profissional"))%>%
  filter(!(CodigoDoPrograma %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1")))%>%
  filter(AnoBase == 2016) %>%
  lm(MediaArtisticaTecnica ~ Modalidade, data = .)%>%
  summary()

data.tableau %>%
  filter(Nivel %in% c("Mestrado", "Mestrado Profissional"))%>%
  filter(!(CodigoDoPrograma %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1")))%>%
  filter(AnoBase == 2016) %>%
  group_by(Modalidade) %>% 
  summarise_at( .vars = colnames(.)[c(81,83)], funs(mean,median,sd,IQR))%>%
  View()
  
data.production %>%
  filter(Nivel %in% c("MESTRADO", "MESTRADO PROFISSIONAL")) %>%
  filter(!(CodPpg %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1")))%>%
  filter(Natureza == "TRABALHO COMPLETO") %>%
  group_by(Modalidade, Estrato) %>%
  summarise(number = n())

data.production %>%
  filter(Nivel %in% c("MESTRADO", "MESTRADO PROFISSIONAL")) %>%
  filter(!(CodPpg %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1")))%>%
  filter(Natureza == "TRABALHO COMPLETO") %>%
  ggplot(., aes(x = Estrato, fill = Modalidade)) + 
  geom_bar(alpha = 0.4, position = "dodge") + theme_bw()

data.production %>% 
  filter(Nivel %in% c("MESTRADO", "MESTRADO PROFISSIONAL")) %>%
  filter(!(CodPpg %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1")))%>%
  filter(Natureza == "TRABALHO COMPLETO") %>%
  group_by(Modalidade, Estrato) %>%
  summarise(total = n())


#######################
# Mosaic exploration
#######################

# Mosaic of production type per modality (Base R)
data.production.tableau %>%
  filter(Nivel %in% c("MESTRADO", "MESTRADO PROFISSIONAL")) %>%
  mosaicplot(Tipo ~ Modalidade, data = .)

# Chi-sq test
table(data.production.tableau$Modalidade, data.production.tableau$Tipo)
chisq.test(table(data.production.tableau$Modalidade, data.production.tableau$Tipo))$expected
chisq.test(table(data.production.tableau$Modalidade, data.production.tableau$Tipo))$residuals

# Advanced script, generalized into a function, including chi-squared statistics
mosaicGG_stat <- function(data, X, FILL) {
  
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  DF_melted <- DF_melted %>%
    group_by(X) %>%
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
                          xmax = xmax, fill = residual)) +
    geom_rect(col = "white") +
    geom_text(aes(x = xtext, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Indivíduos", expand = c(0,0)) +
    scale_y_continuous("Proporção", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}

# Mosaic of production type per modality
data.production.tableau %>%
  filter(Nivel %in% c("MESTRADO", "MESTRADO PROFISSIONAL")) %>%
  filter(!(CodPpg %in% c("31001017155P1", "33283010001P5", "23001011069P5", "31075010001P2", "41002016026P1"))) -> data.production.filtered
  
mosaicGG_stat(data.production.tableau, X = "Tipo", FILL = "Modalidade")

# Chi-sq test
  table(data.production.filtered$Modalidade, data.production.filtered$Tipo)
  chisq.test(table(data.production.filtered$Modalidade, data.production.filtered$Tipo))$expected
  chisq.test(table(data.production.filtered$Modalidade, data.production.filtered$Tipo))$residuals
  
  
############################
# Data translation
############################

translate.tmp <- data.frame(read_excel("Dados/GlossarioIngles.xlsx", col_names = TRUE))

for (i in c(3,4,6,7,8)) {
  data.production.tableau[,i] <- as.factor(lookup(data.production.tableau[,i], translate.tmp[, 1:2], key.reassign = NULL, missing = NULL))
}

summary(data.production.tableau)

for (i in c(3,4,6,7)) {
  data.tableau[,i] <- as.factor(lookup(data.tableau[,i], translate.tmp[, 1:2], key.reassign = NULL, missing = NULL))
}

summary(data.tableau)


############################
# Exporting data
############################

write_excel_csv(data.production, path = "Dados/data_production.csv", na = "") 
write_excel_csv(data.production.tableau, path = "Dados/data_production_tableau.csv", na = "") 
write_excel_csv(data.program, path = "Dados/data_program.csv", na = "") 
write_excel_csv(data.researcher, path = "Dados/data_researcher.csv", na = "") 
write_excel_csv(data.additional, path = "Dados/data_additional.csv", na = "") 
write_excel_csv(data.tableau, path = "Dados/data_tableau.csv", na = "") 