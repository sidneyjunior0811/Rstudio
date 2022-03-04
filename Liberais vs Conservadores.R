library(readr)
library(tidyverse) 
library(plyr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(stringr)
library(tidyr)

df=  read.csv2(file.choose(), sep = ',')

tratamento = df %>% 
  rename(c('Title' = 'Titulo','Political.Lean' = 'Inclinacao_politica', 'Score' = 'Pontuacao', 'Id' = 'ID', 'Subreddit' = 'Subreddit',
           'URL' = 'URL', 'Num.of.Comments' = 'Numero_comentarios', 'Text' = 'Texto', 'Date.Created' = 'Data_criacao'))

# Liberal 

tratamento %>% 
  group_by(Subreddit, Inclinacao_politica) %>% 
  distinct() %>% 
  dplyr:: summarise(
    Contagem = sum(Pontuacao)/1000
  ) %>% 
  filter(Inclinacao_politica == 'Liberal') %>% 
  arrange(-Contagem) %>% 
  ggplot(aes(x = Subreddit ,y = Contagem, fill = Subreddit, label = Contagem))+
  geom_col()+
  labs(title = "Liberal", x = NULL, y = "Comentários")+
  geom_label(alpha = 0.5, size = 2)+
  theme_classic()+
  coord_flip()

# Conservative

tratamento %>% 
  group_by(Subreddit, Inclinacao_politica) %>% 
  distinct() %>% 
  dplyr:: summarise(
    Contagem = sum(Numero_comentarios)/100
  ) %>% 
  filter(Inclinacao_politica == 'Conservative') %>% 
  arrange(-Contagem) %>% 
  ggplot(aes(x = Subreddit ,y = Contagem, fill = Subreddit, label = Contagem))+
  geom_col()+
  geom_label(alpha = 0.5, size = 3)+
  labs(title = "Conservate", x = NULL, y = "Comentários")+
  theme(legend.position = c(1,1))+
  coord_flip()+
  theme_classic()
