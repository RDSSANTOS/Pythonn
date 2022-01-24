# 1a Aula de Mineração de Texto
# 1) Métrica básica em Análise de Texto
# 1.1) Tokenização de Texto e Contagem de Palavras

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("tidytext")
install.packages("janeaustenr")
install.packages("gutenbergr")
install.packages("tm")
install.packages("topicmodels")

install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(janeaustenr)
library(gutenbergr)
library(topicmodels)
library(reshape2)
setwd("C:/Users/rdsf/OneDrive/�rea de Trabalho/GV/Midias Socias e Minera��o de Texto/Grupo/Tarefa 2")

stop_ESP = read.csv("Stop_Esp.csv", sep = ";", header = T)
sent_ESP = read.csv("Sentimentos.csv", sep = ";", header = T)


write.csv(miguel)
miguel2 = read.csv("miguel.csv", sep = ",", header = T)

miguel3 = read_excel("miguel.xlsx")

#####################################
miguel_cerv = c(2000, 57955, 61202, 15115)
gutenberg_download(miguel_cerv) -> miguel


miguel %>% 
  unnest_tokens(input="text", output="word") %>% 
  mutate(word = str_extract(word, regex("[a-z']+")) ) %>% 
  anti_join(stop_ESP) %>% 
  filter(!is.na(word)) %>% 
  count(word) %>% 
  top_n(10) %>% 
  arrange(desc(n)) -> dados_M; dados_M


###Cervantes - Sentimento######################################

miguel %>% group_by(gutenberg_id) %>%
  mutate(linha80 = row_number() %/% 80) %>%
  ungroup() %>%
  unnest_tokens(input=text, output=word) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  inner_join(sent_ESP) %>%
  count(gutenberg_id, linha80, sentiment) %>%
  spread(key=sentiment, value=n) %>%
  mutate(net_sent = Positive - Negative) %>%
  select(gutenberg_id, linha80, net_sent) -> dadosMM; dadosMM

ggplot(dadosMM, aes(x=linha80, y=net_sent)) + 
  geom_col(aes(color=gutenberg_id)) +  
  facet_wrap(~gutenberg_id, scales="free_x", ncol=2) +
  theme(legend.position='none')


miguel3 %>% group_by(gutenberg_id) %>%
  mutate(linha80 = row_number() %/% 80) %>%
  ungroup() %>%
  unnest_tokens(input=text, output=word) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  inner_join(sent_ESP) %>%
  count(gutenberg_id, linha80, sentiment) %>%
  spread(key=sentiment, value=n) %>%
  mutate(net_sent = Positive - Negative) %>%
  select(gutenberg_id, linha80, net_sent) -> dadosM; dadosM

ggplot(dadosM, aes(x=linha80, y=net_sent)) + 
  geom_col(aes(color=gutenberg_id)) +  
  facet_wrap(~gutenberg_id, scales="free_x", ncol=2) +
  theme(legend.position='none')

###Cervantes - Sentimento######################################


##############Cervantes - Zipf#####################################
miguel3 %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  group_by(gutenberg_id) %>% count(word) %>% 
  mutate(tf = n/sum(n)) %>%
  arrange(gutenberg_id, desc(tf)) %>%
  mutate(rank = row_number()) %>%
  select(gutenberg_id, word, tf, rank) %>%
  ungroup() -> miguelzip; miguelzip

miguelzip %>% 
  ggplot(aes(x=rank, y=tf)) + geom_line(aes(color=gutenberg_id)) +
  scale_x_log10() + scale_y_log10()

miguel3 %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  anti_join(stop_ESP) %>% 
  filter(!is.na(word)) %>%
  group_by(gutenberg_id) %>% count(word) %>% 
  mutate(tf = n/sum(n)) %>%
  arrange(gutenberg_id, desc(tf)) %>%
  mutate(rank = row_number()) %>%
  select(gutenberg_id, word, tf, rank) %>%
  ungroup() -> miguelzip2; miguelzip2

miguelzip2 %>% 
  ggplot(aes(x=rank, y=tf)) + geom_line(aes(color=gutenberg_id)) +
  scale_x_log10() + scale_y_log10()
miguelzip2 %>% lm(formula = log(tf) ~ log(rank)) -> modelo_cervantes2
###############Cervantes - Zipf#################################


###############Cervantes - IDF#################################
miguel3 %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  group_by(gutenberg_id) %>% count(word) %>%
  ungroup() %>% bind_tf_idf(term=word, document=gutenberg_id, n=n) %>%
  group_by(gutenberg_id) %>% arrange(gutenberg_id, desc(tf_idf)) %>% 
  mutate(rank=row_number()) %>% ungroup() %>%
  filter(rank<=10) %>%
  ggplot(aes(x=reorder(word,tf_idf), y=tf_idf)) +
  geom_col() +
  facet_wrap(~gutenberg_id, scales='free') + coord_flip() 


###############Cervantes - IDF#################################


###############Cervantes - Topic Modeling#################################
miguel3 %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  group_by(gutenberg_id) %>% count(word) %>%
  ungroup()  -> miguelTM

cast_dtm(miguelTM, document = gutenberg_id, term = word, value = n)

LDA(miguel3, k=2, control = list(seed=1234)) -> miguel_lda
###############Cervantes - Topic Modeling#################################

miguel3 %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  anti_join(stop_ESP) %>% 
  filter(!is.na(word)) %>%
  group_by(gutenberg_id) %>% count(word) %>%
  ungroup()  -> miguelLDA2

migueLDA3 = cast_dtm(miguelLDA2, document = gutenberg_id, term = word, value = n)



LDA(migueLDA3, k=2, control = list(seed=1234)) -> ap_ldaMiguel

tidy(ap_ldaMiguel, matrix="beta") %>%
  mutate(topic = ifelse(topic == 1, "topic1", "topic2")) %>%
  group_by(topic) %>% 
  arrange(topic, desc(beta)) %>%
  mutate(rank = row_number()) %>% ungroup() %>%
  filter(rank<=10) %>%
  ggplot(aes(x=reorder(term, beta), y=beta)) +
  facet_wrap(~topic, scales = 'free_y') + coord_flip() + geom_col()

tidy(ap_lda, matrix="beta") %>%
  mutate(topic = ifelse(topic == 1, "topic1", "topic2")) %>%
  group_by(topic) %>% 
  arrange(topic, desc(beta)) %>%
  mutate(rank = row_number()) %>% ungroup() %>%
  filter(rank<=10) %>%
  ggplot(aes(x=reorder(term, beta), y=beta)) +
  facet_wrap(~topic, scales = 'free_y') + coord_flip() + geom_col()


tidy(ap_lda, matrix="beta") %>%
  mutate(topic = ifelse(topic == 1, "topic1", "topic2")) %>%
  spread(key=topic, value = beta) %>%
  mutate(beta2 = log(topic1/topic2,2)) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  gather(key = topic, value = beta, c("topic1", "topic2")) %>%
  group_by(topic) %>% select(topic, term, beta2) %>%
  mutate(beta2 = abs(beta2)) %>%
  arrange(topic, desc(beta2)) %>% mutate(rank = row_number())

tidy(ap_lda, matrix="beta") %>%
  mutate(topic = paste("topico", topic, sep="")) %>%
  spread(key = topic, value = beta) %>%
  filter(topico1>0.001 | topico2>0.001) %>%
  mutate(beta_spread = log10(topico2/topico1)) %>%
  group_by(topico = beta_spread > 0) %>%
  top_n(10, abs(beta_spread)) %>%
  ggplot(aes(x=reorder(term, beta_spread), y=beta_spread)) +
  geom_col() + coord_flip()
###############Cervantes - Topic Modeling#################################

# Um exemplo de tokenização
textos <- tibble(linha = c(1,2,3,4),
                 texto = c("Naquela madrugada de abril de 1745, o pe. Alonzo acordou angustiado. Seu espírito relutou por",
                           "alguns segundos, emaranhado nas malhas do sonho, como um peixe que se debate na rede, na",
                           "ânsia de voltar a seu elemento natural. Por fim deslizou para a água, mergulhou e ficou imóvel",
                           "naquele poço quadrado, escuro e frio."))

textos %>% unnest_tokens(input="texto", output="word", 
                         token='words', to_lower=TRUE, drop=TRUE) %>% count(word)

# 1o Exercício: determinação das 10 palavras, não stopwords, mais utilizadas por Jane Austen
austen_books() %>% 
  unnest_tokens(input="text", output="word") %>% 
  select(-book) %>% 
  mutate(word = str_extract(word, regex("[a-z']+")) ) %>% 
  anti_join(stop_words) %>% 
  filter(!is.na(word)) %>% 
  count(word) %>% 
  top_n(10) %>% 
  arrange(desc(n)) -> dados; dados

ggplot(dados, aes(x=reorder(word,n), y=n)) +
  geom_col() + coord_flip()


# 2) Aprendizado Não Supervisionado: A máquina irá criar uma
#    saida para nós.
# 2.1) Correlação entre autores

# 1o Baixar os textos de Jane Austen, Wells e Bronte
austen_books() %>% unnest_tokens(input=text, output=word) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  anti_join(stop_words) %>% 
  filter(!is.na(word)) %>%
  select(word) %>%
  mutate(autor = "Miguel") %>%
  count(autor, word) %>%
  mutate(tf = n/sum(n)) %>%
  select(-n) -> austen; austen

baixa_autor <- function(dados, nome="ausente"){
  dados %>% unnest_tokens(input=text, output=word) %>%
    mutate(word = str_extract(word, regex("[a-z']+"))) %>%
    anti_join(stop_words) %>% filter(!is.na(word)) %>% 
    select(word) %>% mutate(autor=nome) %>%
    count(autor, word) %>% mutate(tf = n/sum(n)) %>%
    select(-n) -> dados2
  return(dados2) }

austen_books() -> austen2
baixa_autor(austen2,"austen") -> austen; austen

gutenberg_download(c(35, 36, 159, 5230)) -> wells2
baixa_autor(wells2,"wells") -> wells; wells

gutenberg_download(c(767, 969, 1260, 9182, 768)) -> bronte2
baixa_autor(bronte2,"bronte") -> bronte; bronte 

rbind(austen, wells, bronte) %>% spread(key=autor, value=tf) %>%
  ggplot(aes(x=log(bronte), y=log(austen))) + geom_point()

rbind(austen, wells, bronte) %>% spread(key=autor, value=tf) %>%
  ggplot(aes(x=log(wells), y=log(austen))) + geom_point()

rbind(austen, wells, bronte) %>% 
  spread(key=autor, value=tf) %>%
  filter(!is.na(austen), !is.na(bronte)) %>%
  {cor(.$austen,.$bronte)} # 76%

rbind(austen, wells, bronte) %>% 
  spread(key=autor, value=tf) %>%
  filter(!is.na(austen), !is.na(wells)) %>%
  {cor(.$austen,.$wells)} # 42%

rbind(austen, wells, bronte) %>%
  spread(key=autor, value=tf) %>%
  gather(key=autor, value=tf, c(bronte, wells)) %>%
  ggplot(aes(x=log(austen), y=log(tf))) + 
  geom_point() + facet_wrap(~autor)

#3o Exercício - Análise de Sentimentos

get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc")

austen_books() %>% group_by(book) %>%
  mutate(linha80 = row_number() %/% 80) %>%
  ungroup() %>%
  unnest_tokens(input=text, output=word) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, linha80, sentiment) %>%
  spread(key=sentiment, value=n) %>%
  mutate(net_sent = positive - negative) %>%
  select(book, linha80, net_sent) -> dados; dados

ggplot(dados, aes(x=linha80, y=net_sent)) + 
  geom_col(aes(color=book)) +  
  facet_wrap(~book, scales="free_x", ncol=2) +
  theme(legend.position='none')


#4o Exercicio - Lei de Zipf
# Suponha um texto longo, escrito por qualquer autor, em qualquer lingua.
# Este texto sera tokenizado e feita a contagem das palavras (stopwords ou nao)
# Em seguida sera calculada a term frenquency (proporcao de uso) de cada
# palavra. As palavras serao postas em ordem decrescente de term frequency e 
# sera indicado o ranking de cada palavra correspondente.

# O que é esperado será a diminuição da TF a medida que o rank da palavra
#  aumenta. Isto é decorrente da própria definição de rank. O que não esperado
#  é que conhecendo apenas uma TF, para um rank específico, seja possível prever
#  as próximas TFs com altíssima precisão. A relação que será observada é a 
#  seguinte: TF_hat = TF(rank=10)/rank (TF(rank=10) é um valor constante)

#      TF  =     const1/(rank^n)
#  log(TF) = log(const1/(rank^n))
#  log(TF) = log(const1) - log(rank^n)
#  log(TF) =      const2 -n.log(rank)
#        y =           a -b.x 

austen_books() %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  group_by(book) %>% count(word) %>% 
  mutate(tf = n/sum(n)) %>%
  arrange(book, desc(tf)) %>%
  mutate(rank = row_number()) %>%
  select(book, word, tf, rank) %>%
  ungroup() -> austen; austen

austen %>% 
  ggplot(aes(x=rank, y=tf)) + geom_line(aes(color=book)) +
  scale_x_log10() + scale_y_log10()

# TF = K.rank^(-n)
# log(TF) = log(K)-n.log(rank)
# Um teste possível, o qual certamente poderia gerar um 
#  paper e até mesmo um trabalho de pós-graduação stricto sensu
#  Será que as constantes log(K) e n são universais da espécie humana?
#                                        dependentes da lingua?
#                                        dependentes do autor?


#5o Exercício: TF-IDF
# TF - Term Frequency (n.da palavra no documento/n total no documento)
# IDF - Inverse Document Frequency ( ln[no.de docs/no.de docs com a palavra] )
# TF-IDF - TF * IDF

austen_books() %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) %>%
  group_by(book) %>% count(word) %>%
  ungroup() %>% bind_tf_idf(term=word, document=book, n=n) %>%
  group_by(book) %>% arrange(book, desc(tf_idf)) %>% 
  mutate(rank=row_number()) %>% ungroup() %>%
  filter(rank<=10) %>%
  ggplot(aes(x=reorder(word,tf_idf), y=tf_idf)) +
  geom_col() +
  facet_wrap(~book, scales='free') + coord_flip() 

view("AssociatedPress")
library("AssociatedPress")
data("AssociatedPress")
AssociatedPress

LDA(AssociatedPress, k=2, control = list(seed=1234)) -> ap_lda

LDA(austenlad, k=2, control = list(seed=1234)) -> ap_ldaausten

tidy(ap_ldaausten, matrix="beta") %>%
  mutate(topic = ifelse(topic == 1, "topic1", "topic2")) %>%
  group_by(topic) %>% 
  arrange(topic, desc(beta)) %>%
  mutate(rank = row_number()) %>% ungroup() %>%
  filter(rank<=10) %>%
  ggplot(aes(x=reorder(term, beta), y=beta)) +
  facet_wrap(~topic, scales = 'free_y') + coord_flip() + geom_col()

tidy(ap_lda, matrix="beta") %>%
  mutate(topic = ifelse(topic == 1, "topic1", "topic2")) %>%
  group_by(topic) %>% 
  arrange(topic, desc(beta)) %>%
  mutate(rank = row_number()) %>% ungroup() %>%
  filter(rank<=10) %>%
  ggplot(aes(x=reorder(term, beta), y=beta)) +
  facet_wrap(~topic, scales = 'free_y') + coord_flip() + geom_col()


tidy(ap_lda, matrix="beta") %>%
  mutate(topic = ifelse(topic == 1, "topic1", "topic2")) %>%
  spread(key=topic, value = beta) %>%
  mutate(beta2 = log(topic1/topic2,2)) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  gather(key = topic, value = beta, c("topic1", "topic2")) %>%
  group_by(topic) %>% select(topic, term, beta2) %>%
  mutate(beta2 = abs(beta2)) %>%
  arrange(topic, desc(beta2)) %>% mutate(rank = row_number())

tidy(ap_lda, matrix="beta") %>%
  mutate(topic = paste("topico", topic, sep="")) %>%
  spread(key = topic, value = beta) %>%
  filter(topico1>0.001 | topico2>0.001) %>%
  mutate(beta_spread = log10(topico2/topico1)) %>%
  group_by(topico = beta_spread > 0) %>%
  top_n(10, abs(beta_spread)) %>%
  ggplot(aes(x=reorder(term, beta_spread), y=beta_spread)) +
  geom_col() + coord_flip()

tidy(AssociatedPress) %>%
  filter(document==6) %>%
  arrange(desc(count))

gutenberg_works() %>% View()
gutenberg_download(c(36,164, 42671, 1400), meta_fields=c('title')) -> livros



#######Item2#######

gutenberg_download(c(2000, 57955, 61202, 15115)) -> miguel
miguel$book[miguel$book=='2000']<-"Don Quijote"
miguel$book[miguel$book=='57955']<-"Los entremeses"
miguel$book[miguel$book=='61202']<-"Novelas Ejemplares"
miguel$book[miguel$book=='15115']<-"Novelas y Teatro"
miguel %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  anti_join(stop_spanish)%>%filter(!is.na(word)) %>%
  group_by(book) %>% count(word) %>%
  mutate(tf = n/sum(n)) %>%
  arrange(book, desc(tf)) %>%
  mutate(rank = row_number()) %>%
  select(book, word, tf, rank) %>%
  ungroup() -> miguel; miguel

gutenberg_download(c(54873, 11484, 4791, 800)) -> 
colnames(verne2)<-c('book','text')
