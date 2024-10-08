library(tidyverse)
library(onsvplot)
library(FactoMineR)
library(factoextra)

# Puxando os dados ----
url2007 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2007.zip"
url2008 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2008.zip"
url2014 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2014.zip"
url2015 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2015.zip"
url2016 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2016_atual.zip"
url2017 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2017.zip"
url2018 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2018.zip"
url2019 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2019.zip"
url2020 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2020.zip"
url2021 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2021.zip"
url2022 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2022.zip"
url2023 <- "https://github.com/ONSV/prfdata/raw/main/data-raw/acidentes2023.zip"

#download.file(url2007, "acidentes2007.zip")
#download.file(url2008, "acidentes2008.zip")
#download.file(url2014, "acidentes2014.zip")
#download.file(url2015, "acidentes2015.zip")
#download.file(url2016, "acidentes2016_atual.zip")
#download.file(url2017, "acidentes2017.zip")
#download.file(url2018, "acidentes2018.zip")
#download.file(url2019, "acidentes2019.zip")
#download.file(url2020, "acidentes2020.zip")
#download.file(url2021, "acidentes2021.zip")
#download.file(url2022, "acidentes2022.zip")
#download.file(url2023, "acidentes2023.zip")

#unzip(zipfile = "acidentes2007.zip")
#unzip(zipfile = "acidentes2008.zip")
#unzip(zipfile = "acidentes2014.zip")
#unzip(zipfile = "acidentes2015.zip")
#unzip(zipfile = "acidentes2016_atual.zip")
#unzip(zipfile = "acidentes2017.zip")
#unzip(zipfile = "acidentes2018.zip")
#unzip(zipfile = "acidentes2019.zip")
#unzip(zipfile = "acidentes2020.zip")
#unzip(zipfile = "acidentes2021.zip")
#unzip(zipfile = "acidentes2022.zip")
#unzip(zipfile = "acidentes2023.zip")

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

# Selecionando todos os tipos de caminhões ----

dados_abs_caminhao <- dados %>% 
  filter(tipo_veiculo == "Caminh\xe3o"|
           tipo_veiculo == "Caminh\xe3o-Tanque"|
           tipo_veiculo == "Caminh\xe3o-trator"|
           tipo_veiculo == "Caminh\xe3o-Trator")

# Selecionando ano de fabricação ----
dados_abs_antes <- 
  dados_abs_caminhao %>% 
  filter(periodo == "antes") %>% 
  filter(ano_fabricacao_veiculo %in% c(1936:2008))

dados_abs_depois <- 
  dados_abs_caminhao %>% 
  filter(periodo == "depois") %>% 
  filter(ano_fabricacao_veiculo %in% c(2014:2023))

dados_abs_caminhao <- bind_rows(dados_abs_antes, dados_abs_depois)

# Seleção do estado físico da vítima ----
dados_abs_caminhao <- dados_abs_caminhao %>% 
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

dados_abs_caminhao$classificacao_acidente[dados_abs_caminhao$classificacao_acidente == ""] <- "ignorado"

dados_abs_caminhao <- dados_abs_caminhao %>% 
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

dados_abs_caminhao$estado_fisico[dados_abs_caminhao$estado_fisico == ""] <- "ignorado"


dados_abs_ferido_leve <- 
  dados_abs_caminhao %>% 
  filter(estado_fisico == "ferido leve")

dados_abs_ferido_grave <- 
  dados_abs_caminhao %>% 
  filter(estado_fisico == "ferido grave")

dados_abs_morto <- 
  dados_abs_caminhao %>% 
  filter(estado_fisico == "morto")

dados_abs_ileso <- 
  dados_abs_caminhao %>% 
  filter(estado_fisico == "ileso" & 
           (classificacao_acidente == "com vitimas fatais"|
              classificacao_acidente == "com vitimas feridas"))

dados_abs_caminhao <- bind_rows(dados_abs_ferido_leve,dados_abs_ferido_grave,dados_abs_morto,dados_abs_ileso)
