library(tidyverse)
library(onsvplot)
library(FactoMineR)
library(factoextra)

acidentes2007 <- read.csv('Data-raw/acidentes2007.csv' )
acidentes2008 <- read.csv('Data-raw/acidentes2008.csv' )
acidentes2014 <- read.csv('Data-raw/acidentes2014.csv' )
acidentes2015 <- read.csv('Data-raw/acidentes2015.csv')
acidentes2016 <- read.csv('Data-raw/acidentes2016_atual.csv', sep = ";")
acidentes2017 <- read.csv('Data-raw/acidentes2017.csv', sep = ";")
acidentes2018 <- read.csv('Data-raw/acidentes2018.csv', sep = ";")
acidentes2019 <- read.csv('Data-raw/acidentes2019.csv', sep = ";")
acidentes2020 <- read.csv('Data-raw/acidentes2020.csv', sep = ";")
acidentes2021 <- read.csv('Data-raw/acidentes2021.csv', sep = ";")
acidentes2022 <- read.csv('Data-raw/acidentes2022.csv', sep = ";")
acidentes2023 <- read.csv('Data-raw/acidentes2023.csv', sep = ";")

# Adicionando  a coluna com o ano em cada data frame ----
acidentes2007 <- acidentes2007 %>% 
  mutate(ano = 2007)
acidentes2008 <- acidentes2008 %>% 
  mutate(ano = 2008)
acidentes2014 <- acidentes2014 %>% 
  mutate(ano = 2014)
acidentes2015 <- acidentes2015 %>% 
  mutate(ano = 2015)
acidentes2016 <- acidentes2016 %>% 
  mutate(ano = 2016)
acidentes2017 <- acidentes2017 %>% 
  mutate(ano = 2017)
acidentes2018 <- acidentes2018 %>% 
  mutate(ano = 2018)
acidentes2019 <- acidentes2019 %>% 
  mutate(ano = 2019)
acidentes2020 <- acidentes2020 %>% 
  mutate(ano = 2020)
acidentes2021 <- acidentes2021 %>% 
  mutate(ano = 2021)
acidentes2022 <- acidentes2022 %>% 
  mutate(ano = 2022)
acidentes2023 <- acidentes2023 %>% 
  mutate(ano = 2023)

# Mudando a classificação das variáveis para permitir unir todos data frames ----
acidentes2007$km <- as.numeric(acidentes2007$km)  
acidentes2007$br <- as.integer(acidentes2007$br)
acidentes2007$id_veiculo <- as.numeric((acidentes2007$id_veiculo))
acidentes2007$ano_fabricacao_veiculo <- as.numeric(acidentes2007$ano_fabricacao_veiculo)

acidentes2008$km <- as.numeric(acidentes2008$km)
acidentes2008$br <- as.integer(acidentes2008$br)
acidentes2008$id_veiculo <- as.numeric((acidentes2008$id_veiculo))
acidentes2008$ano_fabricacao_veiculo <- as.numeric(acidentes2008$ano_fabricacao_veiculo)

acidentes2014$id_veiculo <- as.numeric(acidentes2014$id_veiculo)
acidentes2014$ano_fabricacao_veiculo <- as.numeric(acidentes2014$ano_fabricacao_veiculo)

acidentes2015$id_veiculo <- as.numeric(acidentes2015$id_veiculo)
acidentes2015$ano_fabricacao_veiculo <- as.numeric(acidentes2015$ano_fabricacao_veiculo)

acidentes2016$km <- as.numeric(acidentes2016$km)

acidentes2017$km <- as.numeric(acidentes2017$km)

acidentes2018$km <- as.numeric(acidentes2018$km)

acidentes2019$km <- as.numeric(acidentes2019$km)

acidentes2020$km <- as.numeric(acidentes2020$km)
acidentes2020$latitude <- as.character(acidentes2020$latitude)
acidentes2020$longitude <- as.character(acidentes2020$longitude)

acidentes2021$km <- as.numeric(acidentes2021$km)

acidentes2022$km <- as.numeric(acidentes2022$km)

acidentes2023$km <- as.numeric(acidentes2023$km)

# Unindo todos os dataframes em uma base de dados ----
dados <- bind_rows(acidentes2007,acidentes2008,
                   acidentes2014,acidentes2015,
                   acidentes2016,acidentes2017,acidentes2018,
                   acidentes2019,acidentes2020,acidentes2021,
                   acidentes2022,acidentes2023)
# Criando coluna antes e depois da obrigatoriedade  
dados$periodo <- ifelse(
  dados$ano <= 2008,
  "antes",
  "depois"
)


dados_abs_bicleta <- dados %>%                        # TESTANDO ABSURDO (bicicleta com abs)
  filter(tipo_veiculo == "Bicicleta")

dados_abs_antes1 <- 
  dados_abs_bicleta %>% 
  filter(periodo == "antes") %>% 
  filter(ano_fabricacao_veiculo %in% c(1936:2008))

dados_abs_depois1 <- 
  dados_abs_bicleta %>% 
  filter(periodo == "depois") %>% 
  filter(ano_fabricacao_veiculo %in% c(2014:2023))

dados_abs_bicleta <- bind_rows(dados_abs_antes1, dados_abs_depois1)

dados_abs_bicleta <- dados_abs_bicleta %>% 
  mutate(classificacao_acidente = dplyr::recode(classificacao_acidente,
                                                "Sem V\xedtimas" = "sem vitimas",
                                                "Sem V\xedtimas        " = "sem vitimas",
                                                "Com V\xedtimas Fatais " = "com vitimas fatais",
                                                "Com V\xedtimas Fatais" = "com vitimas fatais",
                                                "Com V\xedtimas Feridas" = "com vitimas feridas",
                                                "Ignorado" = "ignorado",
                                                "Ignorado           " = "ignorado",
                                                "(null)" = "ignorado"
  ))


dados_abs_bicleta$classificacao_acidente[dados_abs_bicleta$classificacao_acidente == ""] <- "ignorado"

dados_abs_bicleta <- dados_abs_bicleta %>% 
  mutate(estado_fisico = dplyr::recode(estado_fisico,
                                       "Ileso" = "ileso",
                                       "Ileso       " = "ileso",
                                       "Ferido Leve" = "ferido leve",
                                       "Ferido Leve " = "ferido leve",
                                       "Les\xf5es Leves" = "ferido leve",
                                       "Ferido Grave" = "ferido grave",
                                       "Les\xf5es Graves" = "ferido grave",
                                       "Morto" = "morto",
                                       "Morto       " = "morto",
                                       "\xd3bito" = "morto",
                                       "Ignorado" = "ignorado",
                                       "Ignorado    " = "ignorado",
                                       "N\xe3o Informado" = "ignorado",
                                       "(null)" = "ignorado"
  ))

dados_abs_bicleta$estado_fisico[dados_abs_bicleta$estado_fisico == ""] <- "ignorado"

dados_abs_ferido_leve <- 
  dados_abs_bicleta %>% 
  filter(estado_fisico == "ferido leve")

dados_abs_ferido_grave <- 
  dados_abs_bicleta %>% 
  filter(estado_fisico == "ferido grave")

dados_abs_morto <- 
  dados_abs_bicleta %>% 
  filter(estado_fisico == "morto")

dados_abs_ileso <- 
  dados_abs_bicleta %>% 
  filter(estado_fisico == "ileso" & 
           (classificacao_acidente == "com vitimas fatais"|
              classificacao_acidente == "com vitimas feridas"))

dados_abs_bicleta <- bind_rows(dados_abs_ferido_leve,dados_abs_ferido_grave,dados_abs_morto,dados_abs_ileso)

#----

Prop_ef_abs_bici_antes <- as.data.frame(table(dados_abs_bicleta$estado_fisico[dados_abs_bicleta$periodo == "antes"]))
Prop_ef_abs_bici_depois <- as.data.frame(table(dados_abs_bicleta$estado_fisico[dados_abs_bicleta$periodo == "depois"]))

Prop_ef_abs_bici_antes <- 
  Prop_ef_abs_bici_antes %>% 
  mutate(proporção = Freq/sum(Freq)) %>% 
  mutate(variavel = "ABS bicicleta Antes")

Prop_ef_abs_bici_depois <- 
  Prop_ef_abs_bici_depois %>% 
  mutate(proporção = Freq/sum(Freq)) %>% 
  mutate(variavel = "ABS bicicleta Depois")

Prop_ef_abs_bici <- bind_rows(Prop_ef_abs_bici_antes,Prop_ef_abs_bici_depois)

Prop_ef_abs_bici %>% 
  ggplot(aes(x = reorder(Var1, proporção), y = proporção))+
  geom_col(fill = "orange2", color = "black")+
  geom_text(aes(y = proporção, label = scales::percent(proporção)), vjust = -0.5,
            position = position_dodge(width = 1)) +
  facet_wrap(~ variavel)+
  theme_bw()+
  labs(x = "Estado físico", y = "Proporção")

tab_abs_bici <- table(dados_abs_bicleta$estado_fisico, dados_abs_bicleta$periodo)

q2abs_bici <- chisq.test(tab_abs_bici)