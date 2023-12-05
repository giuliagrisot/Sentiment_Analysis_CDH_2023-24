library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(),
               chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
        ungroup()

original_books

tidy_books <- original_books %>%
  unnest_sentences(sentence, text) %>%
  mutate(sentence_n = seq_along(sentence)) %>%
  unnest_tokens(word, sentence, drop = F) %>%
  mutate(token_n = seq_along(word))


tidy_books

tidy_books <- tidy_books %>%
        anti_join(stop_words[1])

tidy_books %>%
        count(word, sort = TRUE)

library(tidyr)
bing <- tidytext::get_sentiments(lexicon = "bing")

bing


janeaustensentiment <- tidy_books %>%
  left_join(bing) %>%
  mutate(sentiment_value = ifelse(!is.na(sentiment), 1, 0)) %>%
  mutate(sentiment_value = ifelse(sentiment == "positive", 1, -1)) %>%
  group_by(sentence, sentiment_value) %>%
  group_by(sentence_n, sentence) %>%
  dplyr::summarize(sum(sentiment_value))

janeaustensentiment