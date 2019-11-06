############################################################################################################
###############################################  Capítulo 1  ###############################################
############################################################################################################

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

library(dplyr)
text_df <- tibble(line = 1:4, text = text)
text_df

# muestra la linea donde está la palabra (formato una-línea-por-palabra)
library(tidytext)
text_df %>%
  unnest_tokens(palabra, text) # (nombre de la columna , texto a analizar)

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books # Base de datos de libros de un autor

# Formato-una-palabra-por-linea
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

# Eliminar palabras muy comunes en inglés
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)


# Palabras más comunes y la cantidad de veces que aparecen
tidy_books %>%
  count(word, sort = TRUE) # sort = FALSE las ordena en orden alfabético

# Visualización de las palabras más comunes
library(ggplot2)
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


library(gutenbergr)
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

# Acceso a algunos libros por su código
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
# Palabras más comunes en estas novelas
tidy_bronte %>%
  count(word, sort = TRUE)

# Frecuencia de cada palabra en los libros de los autores.
library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)





