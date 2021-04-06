library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidytext)
library(wordcloud)

rt <- search_tweets(
  "#rstats", n = 18000, include_rts = FALSE
)

# Dev environments
# apps, en el menu de la izquierda en Products y luego hasta abajo de Premium, Dev Environments, 
# escojer el segundo "Search Tweets: Full ArchiveSandbox" NO se si ya con este funciona siempre

yodecido2020_010100_311224 <- search_fullarchive(
  q = "#yodecido",n = 5000,
  env_name = 'extraccionarxiv',  #esta hay que crearla en la app 
  fromDate = "202001100000", toDate = "202012312359"  #AAAAMMDDHHMM
)
saveRDS(yodecido2020_010100_311224, "yodecido2020_010100_311224.rds")


yodecido2021_010100_03261300 <- search_fullarchive(
  q = "#yodecido",n = 5000,
  env_name = 'extraccionarxiv',
  fromDate = "202101010000", toDate = "202103261300"
)
saveRDS(yodecido2021_010100_03261300, "yodecido2021_010100_03261300.rds")


readRDS("yodecido2020.rds")
readRDS("yodecido2021.rds")

files.rds <- list.files(path = ".", pattern = "*.rds")

all_tweets <- lapply(files.rds, readRDS) %>% bind_rows() %>% distinct()

# names 
# all_tweets$retweet_text 
# all_tweets$hashtags
# all_tweets$text


all_tweets %>%
  group_by(screen_name) %>%
  count() %>% 
  sort(n, partial = c(10, 15))
  ggplot(aes(x = day_tw, y=n)) +
  geom_line()
  
  
all_tweets %>%
  count(screen_name, sort = TRUE) %>%
  filter(n > 50)

# TEXT
tm_stop_words <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                      lexicon = "custom"))

texto_tweets <- all_tweets %>% 
  select(screen_name, text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(tm_stop_words, by = "word")


texto_tweets_c <- texto_tweets %>%
  count(word, sort = TRUE) %>% 
  filter(word != c("https")) %>% 
  filter(word != c("t.co"))


texto_tweets_c %>% 
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#8d99ae") +
  labs(title = "Frecuencia de palabras en tweets #yodecido", 
       x = "FRECUENCIA", y = " ", color = "", size = 40) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "#88398A")) +
  theme(axis.title.y = element_text(size = 18, face="bold", color = "#88398A"),
        axis.title.x = element_text(size = 18, face="bold", color = "#88398A"),
        axis.text = element_text(size = 16, face="bold", color = "#88398A"))


wordcloud(
  words = texto_tweets_c$word,
  freq = texto_tweets_c$n,
  min.freq =100,
  max.words=6000,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)


# all_tweets$retweet_text 
texto_retweets <- all_tweets %>% 
  select(screen_name, retweet_text) %>% 
  unnest_tokens(word, retweet_text) %>% 
  anti_join(tm_stop_words, by = "word")


texto_retweets_c <- texto_retweets %>%
  count(word, sort = TRUE) %>% 
  filter(word != c("https")) %>% 
  filter(word != c("t.co"))


texto_retweets_c %>% 
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "#8d99ae") +
  labs(title = "Frecuencia de palabras en retweets #yodecido", 
       x = "FRECUENCIA", y = " ", color = "", size = 40) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "#88398A")) +
  theme(axis.title.y = element_text(size = 18, face="bold", color = "#88398A"),
        axis.title.x = element_text(size = 18, face="bold", color = "#88398A"),
        axis.text = element_text(size = 16, face="bold", color = "#88398A"))


wordcloud(
  words = texto_retweets_c$word,
  freq = texto_retweets_c$n,
  min.freq =100,
  max.words=6000,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)


