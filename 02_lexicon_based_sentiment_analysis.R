library(tidytext)
library(tidyverse)
library(sjPlot)
library(readtext)
library(syuzhet)

options(stringsAsFactors = F, # do not convert to factor upon loading
        scipen = 999, # do not convert numbers to e-values
        max.print = 200, # stop printing after 200 values
        warn = -1) # as many warnings in R are useless (and annoying), you might want to disable them


theme_set(theme_sjplot2()) # set default ggplot theme to light

# Let's recreate out corpus as of script 1


corpus_sentence <- readtext("corpus/*.txt",
                            encoding = "UTF-8")  %>%
  mutate(doc_id2 = seq_along(doc_id)) %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, drop = T) %>%
  mutate(sentence = str_squish(sentence)) %>% # eliminate unwanted extra spaces
  separate(doc_id, into = c("author", "title", "year"),
           sep = "_",
           remove = T) %>% # and separate the metadata
  rename(doc_id = doc_id2) %>%
  mutate(year = str_remove(str_trim(year, side = "both"), ".txt"))  %>% # and make sure there are no extra spaces before/after the words
  group_by(title) %>%
  mutate(sentence_id = seq_along(sentence)) %>% # this means "sequence along the column sentence"
  ungroup() %>%
  select(doc_id, sentence_id, everything()) %>%
  mutate(year = as.numeric(year))


corpus_token <- unnest_tokens(corpus_sentence,
                              input = "sentence",
                              output = "token",
                              to_lower = T,
                              drop = T) %>%
  group_by(title, sentence_id) %>%
  mutate(token_id = seq_along(token)) %>% # this means "sequence along the column "token"
  ungroup()


# save(corpus_token, corpus_sentence, file = "corpus.RData")

# Module 1: Introduction to Sentiment Analysis

# Introduction to lexicon-based sentiment analysis
# Lexicon-based sentiment analysis involves using predefined dictionaries (lexicons)
# that associate words with sentiment scores. This approach is useful for analysing
# sentiment in text data without the need for training a machine learning model.



## simple SA with syuzhet -----

# syuzhet allows you to run basic SA operations very easily, like extracting the sentiment value per sentence into a vector of values for the text.
# we can try with Woolf's To the Lighthouse

woolf_sentiments <- syuzhet::get_sentiment(
  corpus_sentence$sentence[grepl("Lighthouse", corpus_sentence$title)],
  language = "english"
)

# if you want to check that it did really took values for sentences, you can verify that the number of sentiment values in the federer_sents vector equals the number of sentences in the corpus that has "Lighthouse" as an title:

nrow(corpus_sentence[grepl("Lighthouse", corpus_sentence$title), "sentence"]) == length(woolf_sentiments)

# it should say: TRUE!

# we cal also visualise these data as plots, directly with syuzhet from txt files

simple_plot(woolf_sentiments)

# we can try the same with Jane Eyre

jane_sentiments <- syuzhet::get_sentiment(
  corpus_sentence$sentence[grepl("Jane", corpus_sentence$title)],
  language = "english"
)
simple_plot(jane_sentiments)

# can you see the different emotional arc for the two novels?




remove(woolf_sentiments, jane_sentiments)
gc()

# While this gives you a quick view on the overall sentiment trend, I prefer to have a more "visual" (though more memory intensive) approach, applying the sentiment lexicon directly to our corpus.
# To do that, you have to store the lexicons separately and 'paste' it onto your corpus.

# You can import the lexicons you will use for your sentiment analysis.
# Lexicons are often available as part of several packages.
# for example, you may use the syuzhet package to import the various sentiment lexicons provided within the package syuzhet itself

syuzhet_default <- get_sentiment_dictionary(language = "english")

syuzhet_nrc <- get_sentiment_dictionary(language = "english", dictionary = "nrc")

syuzhet_bing <- get_sentiment_dictionary(language = "english", dictionary = "bing")

syuzhet_afinn <- get_sentiment_dictionary(language = "english", dictionary = "afinn")

# Notice that these lexicons measure different things. unless you want to compare preformance, you might need to decide which lexicon works best for you, and be aware of how it was created.



# Once we decide which lexicon to use, we can examine different aspects. For instance we can se the average sentiment (positive/negative) per novel, as a plot

corpus_token %>%
  left_join(syuzhet_default, by = c("token" = "word")) %>%
  group_by(title, author) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>% # MEAN
  ggplot(aes(reorder(title, -sentiment_value), sentiment_value)) +
  geom_col(aes(fill=author)) +
  ylim(-.2,.2) +
    theme(legend.position = 'bottom', # get rid of legend
        text = element_text(size = 12), # determine fs
          axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12/1.5) # rotate x text
    )

# or as a table

corpus_token %>%
  left_join(syuzhet_default, by = c("token" = "word")) %>%
  group_by(title, author) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) # MEAN


# every lexicon uses different values, and you need to know them in able to examine them. for instance, if we change the lexicon to 'bing', let's see what happens.
# also, you can use the mean value or the sum value, the result will change, and the decision relies on your research question.
# below we try with SUM instead of MEAN (above)

corpus_token %>%
  left_join(syuzhet_bing, by = c("token" = "word")) %>%
  group_by(title, author) %>%
  summarise(sentiment_value = sum(value, na.rm=T)) %>% # SUM
  ggplot(aes(reorder(title, -sentiment_value), sentiment_value)) +
  geom_col(aes(fill=author)) +
    # ylim(-.2,.2) +
    theme(legend.position = 'bottom', # get rid of legend
        text = element_text(size = 12), # determine fs
          axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12/1.5) # rotate x text
    )

# or as a table

corpus_token %>%
  left_join(syuzhet_bing, by = c("token" = "word")) %>%
  group_by(title, author) %>%
  summarise(sentiment_value = sum(value, na.rm=T)) # SUM


# We can also look at how sentiment changes over time in publications by author

corpus_token %>%
  left_join(syuzhet_default, by = c("token" = "word")) %>%
  group_by(year, author, title) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>%
  ggplot(aes(sentiment_value, year)) +
  geom_point(size=3, aes(color=author))

# we could also decide to split view and compare how many 'negative' vs 'positive' tokens we have per novel
# first we have to see how the sentiment lexicon values distribute:

syuzhet_default %>%
  ggplot(aes(value)) +
  geom_histogram()
#
# syuzhet_bing %>%
#   ggplot(aes(value)) +
#   geom_histogram()
#
# syuzhet_afinn %>%
#   ggplot(aes(value)) +
#   geom_histogram()

# 0 is in the middle, so we can add a label 'positive' above 0, neutral = 0 and negative <0

corpus_token %>%
  left_join(syuzhet_default, by = c("token" = "word")) %>%
  mutate(polarity = case_when(
      value > 0 ~ "positive",
      value == 0 ~ "neutral",
      value < 0  ~ "negative"
    )) %>%
  filter(!is.na(value)) %>%
  group_by(title, author, polarity) %>%
  count() %>%
  ggplot() +
  geom_col(aes(title, n, fill=polarity), position = "dodge") +
    theme(legend.position = 'bottom', # get rid of legend
        text = element_text(size = 12), # determine fs
          axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12/1.5) # rotate x text
    )


# or look at the average posiive and negative value


corpus_token %>%
  left_join(syuzhet_default, by = c("token" = "word")) %>%
  mutate(polarity = case_when(
      value > 0 ~ "positive",
      value == 0 ~ "neutral",
      value < 0  ~ "negative"
    )) %>%
  filter(!is.na(value)) %>%
  mutate(value = ifelse(polarity=="negative", -value, value)) %>%
  group_by(title, author, polarity) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>% # MEAN
  ggplot() +
  geom_col(aes(title, sentiment_value, fill=polarity), position = "dodge") +
    theme(legend.position = 'bottom', # get rid of legend
        text = element_text(size = 12), # determine fs
          axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12/1.5) # rotate x text
    )



gc()




# Also, it is important to remember that dictionaries within packages can be updated or change over time (or become unavailable).

# You can then decide that you prefer to store them locally, so that you can be sure they do not change and that you will be able to access them whether you are online or not.

# for example, if you go to the official source of the NRC lexicon developed by Saif Mohammad, https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm , you will be able to download the lexicon in several formats.


# with sentiments, you might be interested in amore qualitative approach
# we can for instance start by looking at the most frequent terms in the corpus, removing stopwords first

stopwords_en <- stop_words %>%
  as_tibble() %>%
  rename(token = word) %>%
  select(token)

corpus_token %>%
  mutate(token = tolower(token)) %>%
  # delete stopwords
  anti_join(stopwords_en) %>%
  # summarize count per word per book
  count(title, author, year, token) %>%
  # get top 15 words per book
  group_by(title, author, year) %>%
  slice_max(order_by = n, n = 15) %>%
  mutate(word = reorder_within(token, n, title)) %>%
  # create barplot
  ggplot(aes(x = word, y = n, fill = author)) +
  geom_col(color = "black") +
  scale_x_reordered() +
  labs(
    title = "Most frequent words in corpus",
    x = NULL,
    y = "Word count"
  ) +
  facet_wrap(facets = vars(title), scales = "free") +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_sjplot()


# Most frequent words associated with discrete emotion used in the corpus (NRC)

corpus_token %>%
  left_join(as_tibble(syuzhet_nrc), by = c("token" = "word")) %>%
  filter(!is.na(sentiment)
           & sentiment != 'positive'
           & sentiment != 'negative') %>%
  # generate frequency count for each word and sentiment
  group_by(sentiment) %>%
  count(token) %>%
  # extract 10 most frequent pos/neg words
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  # prep data for sorting each word independently by facet
  mutate(word = reorder_within(token, n, sentiment)) %>%
  # generate the bar plot
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  # used with reorder_within() to label the axis tick marks
  scale_x_reordered() +
  facet_wrap(facets = vars(sentiment), scales = "free_y") +
  labs(
    title = "Most frequent words associated with discrete emotion used in the corpus (NRC)",
    x = NULL,
    y = "Number of occurences in all seven books"
  ) +
  coord_flip() +
  scale_fill_sjplot()

# Most frequent words associated with sentiment (polarity) used in the corpus (NRC)

corpus_token %>%
  mutate(token = tolower(token)) %>%
  left_join(as_tibble(syuzhet_nrc), by = c("token" = "word")) %>%
  filter(sentiment == 'positive'
           | sentiment == 'negative') %>%
  # generate frequency count for each word and sentiment
  group_by(sentiment) %>%
  count(token) %>%
  # extract 10 most frequent pos/neg words
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 25) %>%
  # prep data for sorting each word independently by facet
  mutate(word = reorder_within(token, n, sentiment)) %>%
  # generate the bar plot
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  # used with reorder_within() to label the axis tick marks
  scale_x_reordered() +
  facet_wrap(facets = vars(sentiment), scales = "free_y") +
  labs(
    title = "Sentiment emotion words used in the corpus (NRC)",
    x = NULL,
    y = "Number of occurences in all seven books"
  ) +
  coord_flip()



# occurrences of words embedding discrete emotions with NRC lexicon
# in a specific novel (Lighthouse)

corpus_token %>%
  mutate(token = tolower(token)) %>%
  filter(title == "Lighthouse") %>% # novel specified (try and change it!)
  left_join(as_tibble(syuzhet_nrc), by = c("token" = "word")) %>%
  filter(sentiment == 'positive'
           | sentiment == 'negative') %>%
  # generate frequency count for each word and sentiment
  group_by(sentiment) %>%
  count(token) %>%
  # extract 10 most frequent pos/neg words
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 25) %>%
  # prep data for sorting each word independently by facet
  mutate(word = reorder_within(token, n, sentiment)) %>%
  # generate the bar plot
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  # used with reorder_within() to label the axis tick marks
  scale_x_reordered() +
  facet_wrap(facets = vars(sentiment), scales = "free_y") +
  labs(
    title = "Sentiment emotion words used in To The Lightouse (NRC)",
    x = NULL,
    y = "Number of occurences in all seven books"
  ) +
  coord_flip()



# Or with actual sentiment values featuring, rather than count, for the most frequently occurring words with a sentiment value

corpus_token %>%
  mutate(token = tolower(token)) %>%
  filter(title == "Lighthouse") %>%
  left_join(as_tibble(syuzhet_default), by = c("token" = "word")) %>%
  # generate frequency count for each word and sentiment
  group_by(token) %>%
  summarize(n = n(),
            value = mean(value)) %>%
  filter(!is.na(value)) %>%

  # extract 10 most frequent pos/neg words
  slice_max(order_by = n, n = 25) %>%
  # prep data for sorting each word independently by facet
  mutate(word = reorder_within(token, n, value)) %>%
  # generate the bar plot
  ggplot() +
  geom_col(aes(word, value, fill = value), show.legend = FALSE) +
  # geom_point(aes(word, n)) +
  # used with reorder_within() to label the axis tick marks
  scale_x_reordered() +
  labs(
    title = "Most frequent sentiment words and their value in To The Lighghouse (NRC)",
    x = NULL,
    y = "Number of occurences in all seven books"
  ) +
  coord_flip()