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