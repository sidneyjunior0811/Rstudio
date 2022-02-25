library(readr)
library(tidyverse) 
library(plyr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(stringr)
library(tidyr)


df = read.csv2(file.choose(),encoding = 'UTF-8', sep = ',')

tratamento=df %>% 
  select(-X) %>% 
  rename(c('brand' = 'Marca', 'vehicle' = 'Veiculo', 'year_model' = 'Modelo_ano', 'fuel' = 'Combustivel',
           'price_reference' = 'Referencia_preço', 'price' = 'Preço')) %>% 
  mutate(Modelo_ano = ifelse(Modelo_ano == '32000','Zero KM', Modelo_ano)) %>% 
  mutate(Preço = gsub('R\\$|\\.|',"", Preço)) %>% 
  mutate(Preço = gsub(',','.',Preço))%>% 
  mutate(Cilindradas = str_extract(Veiculo, '[0-9]\\.[0-9]'))

# Veiculos Automaticos
Veiculo_Aut = tratamento %>% 
  filter(!is.na(Cilindradas)) %>% 
  select(Veiculo) %>% 
  distinct() %>% 
  mutate(Cambio = str_extract(Veiculo, ' Aut\\.'), Cambio) %>% 
  mutate(Cambio = ifelse(Cambio == " Aut.", 'Auto', Cambio))

#Juntando os dois DF
Agrupamento_Veiculo = left_join(tratamento, Veiculo_Aut) %>% 
  mutate(Cambio = !is.na(Cambio == 'Manual')) %>% 
  mutate(Cambio = ifelse(Cambio == "TRUE", "Automatico", Cambio)) %>% 
  mutate(Cambio = ifelse(Cambio == "FALSE", "Manual", Cambio)) %>% 
  mutate(Cambio = as.factor(Cambio))

 
DF_Carro = Agrupamento_Veiculo %>%
  separate(Veiculo, into = 'Nome', sep = ' ', remove = F) %>% 
  mutate(Nome = as.factor(Nome)) %>% 
  mutate(Marca = as.factor(Marca)) %>% 
  mutate(Veiculo = as.factor(Veiculo)) %>% 
  mutate(Combustivel = as.factor(Combustivel)) %>% 
  mutate(Referencia_preço = as.factor(Referencia_preço)) %>% 
  mutate(Cilindradas = as.factor(Cilindradas)) %>% 
  mutate(Preço = as.numeric(Preço))
         