library(dplyr)
library(tidytext)
library(ggplot2)

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")


cri <- c("La patita",
         "De canasto y con rebozo de bolita",
         "Va al mercado",
         "A comprar todas las cosas del mandado",
         "Se va meneando al caminar",
         "Como los barcos en altamar",
         "La patita",
         "Va corriendo y buscando en su bolsita",
         "Centavitos",
         "Para darles de comer a sus patitos",
         "Porque ella sabe que al retornar",
         "Toditos ellos preguntarán",
         "¿Qué me trajiste, mamá cuac cuac?",
         "¿Qué me trajiste cua-rá cuac-cuac?",
         "La patita",
         "Como tú",
         "De canasto y con rebozo de bolita",
         "Como tú",
         "Se ha enojado",
         "Como tú",
         "Por lo caro que está todo en el mercado",
         "Como no tiene para comprar",
         "Se pasa el día en regatear",
         "Sus patitos",
         "Van creciendo y no tienen zapatitos",
         "Y su esposo",
         "Es un pato sinvergüenza y perezoso",
         "Que no da nada para comer",
         "Y la patita ¿pues qué va a hacer?",
         "Cuando le pidan, contestará",
         "¡Coman mosquitos",
         "Cua-rá cuac-cuac!")


text_df <- tibble(line = 1:4, text = text)
cri_df <- tibble(line = 1:32, texto = cri)

text_df %>%
  unnest_tokens(word, text)

cri_df %>%
  unnest_tokens(palabra, texto)



##############
library(janeaustenr)
library(dplyr)
library(stringr)
library(tm)
library(quanteda)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)


### para quitar palabras stop-words
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) 


#### stop_words en español
tm_stop_words <- bind_rows(data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

cri_df_w <- cri_df %>%
  unnest_tokens(word, texto)

cri_df_w1 <- cri_df_w %>% 
  anti_join(tm_stop_words)


################
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

cri_df_w1 %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#######################################



