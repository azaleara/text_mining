library(textdata)
library(tidyverse)

# se deben bajar los corpus
get_sentiments("afinn")
unique(get_sentiments("afinn")[,2]) # va de -5 a 5

emocion_corpus <- read_csv("13428_2015_700_MOESM1_ESM.csv", 
                           locale = readr::locale(encoding = "latin1"))

emocion_lexicon <- emocion_corpus %>% 
  select(!c(X1, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18,
            `%ValenceRaters`, `%ArousalRaters`))

emocion_lexicon <- emocion_lexicon %>% 
  rename(word = Word)

emocion_lexicon <- emocion_lexicon %>% 
  mutate(
    sentiment = case_when(
      ValenceMean < 4 ~ "negative",
      ValenceMean > 6 ~ "positive",
      ValenceMean >= 4 | ValenceMean <= 6 ~ "neutral"))


# hay más palabras neutrales
emocion_lexicon %>% 
  count(sentiment)



##### book
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


### esto es para 
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# graficar
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")



######## pruebas
querida_df_0 %>% 
  inner_join(emocion_lexicon) %>% 
  count(word, sort = TRUE) 
  
# se une con el lexico emociones y se cuantifica de acuerdo a la valencia
querida_emoc_0 <- querida_df_0 %>% 
  inner_join(emocion_lexicon)

# es más positiva
querida_emoc_0 %>% 
  count(sentiment)

# visualizar las palabras más frecuentes de acuerdo a su valencia 
querida_emoc_0 %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
#  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# se cuenta por renglón 
querida_emoc <- querida_df_0 %>% 
  inner_join(emocion_lexicon) %>% 
  count(index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

querida_emoc %>% 
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE) 

# quitar sentiment y hacerlo long para graficar con los tres niveles
querida_emoc_m <- querida_emoc %>% 
  select(!sentiment) %>% 
  gather(variable, value, -index)


querida_emoc_m %>% 
  count()

querida_emoc_m %>% 
  ggplot(aes(index, value, fill = variable)) +
  geom_bar(stat='identity')


## wordcloud todo
wordcloud(
  words = querida_df_00$word,
  freq = querida_df_00$n,
  min.freq =1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)


querida_emoc_0 %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#2a9d8f", "#adb5bd", "#88398A"),
                   max.words = 100)

