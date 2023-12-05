
# Where do I start? Create your corpus_1 and set up your data with R Studio -----



# This is an R script file, created by Giulia (reads like English "Julia")

# Everything written after an hashtag is a comment (normally appears in green). If you don't want to type the hash manually every time, you can type your comments normally and after you finish, with the cursor on the sentence, press ctrl+shift+c. it will turn text into a comment and vice versa.

# Everything else is R code. To execute the code, place the cursor on the corresponding line and press Ctrl+Enter (windows)

# For today's practice, you will not need much knowledge of R: the scripts are provided for you. You will be guided through a simple case exploratory Sentiment Analysis, and then use those same scripts to experiment with data of your choice.
# If you are unfamiliar with R language and basic operations and want to learn more, have a look at the resources at the end of this script.

# before you start, check the working directory!
# you can click on the Files panel, go to the practice_GG folder, and once you are inside click on the little arrow near the "More" button, and select "Set as working directory"


# now we're ready to start!



  
# PS: Have you noticed that there is a little symbol on the top right of this panel, made of little horizontal lines? it is the document outline. if you write a comment and put a series of hashes after it, will become the header of a section, which you can then see in the outline for an easier navigation of the script. You can hide or visualise the outline by clicking on the button.

library(tidyverse)
library(syuzhet)
library(tidytext)
library(sjPlot)
library(wordcloud)
library(readtext)



## set options --------------------

# we can start setting up a few options for our project

options(stringsAsFactors = F, # do not convert to factor upon loading
        scipen = 999, # do not convert numbers to e-values
        max.print = 200, # stop printing after 200 values
        warn = -1) # as many warnings in R are useless (and annoying), you might want to disable them


theme_set(theme_sjplot2()) # set default ggplot theme to light
fs = 12 # default plot font size


## corpus_1 creation -----------------

# in this case, the files are consistently saved as author_title_year.txt, where we use one word only for the author and for the title, and the format YYYY for the year of first publication.

# It is important to be consistent! It can make your life much easier when you deal with many texts.

# we can tell R to look only for txt files in the working directory, and to "unnest" sentences, i.e. to split the texts into setnences for us.

# while having a big corpus_1 is in principle the best solution for quantitative analyses, it also adds a level of complication: a higher processing effort. with 200+ texts, only the dataframe created with tokenization was larger the 5 GB, and RStudio Cloud only supports a limited amount of data.
# For the sake of the workshop, we will use a reduced version of the corpus, with only
# 150 sentences per book

corpus_sentence <- readtext("corpus/*.txt", encoding = "UTF-8")  %>%
  mutate(doc_id2 = seq_along(doc_id)) %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, drop = T) %>%
  mutate(sentence = str_squish(sentence)) %>% # eliminate unwanted extra spaces
  separate(doc_id, into = c("author", "title", "year"), sep = "_", remove = T) %>% # and separate the metadata
  rename(doc_id = doc_id2) %>%
  select(doc_id, sentence_id, everything()) %>%
  mutate(year = str_remove(str_trim(year, side = "both"), ".txt")) %>% # and make sure there are no extra spaces before/after the words
  mutate(year = as.numeric(year)) %>%

  group_by(title) %>%
  mutate(sentence_id = seq_along(sentence)) %>% # this means "sequence along the column sentence"
  ungroup()

# and we might want then to split the text into tokens. we can easily use the unnest_tokens function of tidytext:

corpus_token <- unnest_tokens(corpus,
                              input = "sentence",
                              output = "token",
                              to_lower = T,
                              drop = F) %>%
  group_by(title, sentence) %>%
  mutate(token_id = seq_along(token)) %>% # this means "sequence along the column "token"
  ungroup()


# so let's see how does it look now

head(corpus_token, 10)

# we can also create a word identification number per title per sentence, and a unique word id

corpus_token <- corpus_token %>%
  group_by(title, sentence_id) %>%
  mutate(token_id = seq_along(token)) %>%
  ungroup() %>%
  mutate(unique_word_id = seq_along(token))
  


# let's have a look at out dataset now

head(corpus_token)



# because in this case we do not have chapters data in our dataset, we can arbitrarily assign "fake chapters" to the novels, to see the evolution of sentiment throughout them. (of course if you have that data already present in your dataset you do not need this)

# for the sake of simplicity, let's split the novels into 15 chapters each.

test <- corpus_token %>%
  ungroup() %>%
  group_split(title)

test2 = list()

for (i in 1:length(test)) {
  avg_ch_lenght <- nrow(test[[i]])/15
  r  <- rep(1:ceiling(nrow(test[[i]])/avg_ch_lenght),each=avg_ch_lenght)[1:nrow(test[[i]])]
  test2[[i]] <- split(test[[i]],r)
}


for (i in 1:length(test2)) {
  for (j in 1:length(test2[[i]])) {
    test2[[i]][[j]]$chapter <- paste0(j)
  }
}

test = list()

for (i in 1:length(test2)) {
  test[[i]] <- data.table::rbindlist(test2[[i]])
}

corpus_token <- data.table(rbindlist(test))


remove(test, test2, j,i,r,avg_ch_lenght)



# let's have a look again

head(corpus_token)



# we might want to take off the stopwords



stop_german <- tibble(word = stopwords::stopwords("de"))

stop_german2 <- stop_german
stop_german2$word <- str_to_sentence(stop_german2$word)
stop_german <- bind_rows(stop_german, stop_german2)
remove(stop_german2)
stop_german <- stop_german %>%
  rename(token = word)


# corpus_token overview --------
## plot token frequency ---------------------- 

# now we can have a first look at our corpus_token and see which tokens are most frequent in the novels

corpus_token %>%
  # filter(grepl("keller", doc_id)) %>%
  group_by(token) %>%
  anti_join(stopwords_en) %>% # delete stop words
  count() %>% # summarize count per token per title
  arrange(desc(n)) %>% 
  head(100) #  %>% # highest freq on top
  # view()

corpus_token %>%
  filter(grepl("keller", doc_id)) %>% # let's look only at Keller
  group_by(title, token) %>%
  anti_join(stop_german, by = "token") %>% # delete stop words
  count() %>% # summarize count per token per title
  arrange(desc(n)) %>% # highest freq on top
  group_by(title) %>% # 
  mutate(top = seq_along(token)) %>% # identify rank within group # identify rank within group
  filter(top <= 15) %>% # retain top 15 frequent tokens
  # create barplot
  ggplot(aes(x = -top, fill = title)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure tokens are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = token), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "token count", x = "", # add labels
       title = "Keller: Most frequent tokens throughout the novels") +
  facet_grid(. ~ title) + # separate plot for each title
  coord_flip() + # flip axes
  scale_fill_sjplot()

# relatively unsurprisingly, names of characters are generally the most frequent tokens. To see what other tokens are highly frequent, we can for example import a list of first and last names, so that we can exclude them from the plot.


german_names <- read_delim("Vornamen_2020_Koeln_edited.csv", 
                           delim = ";", escape_double = FALSE, 
                           col_types = cols(anzahl = col_skip(), 
                                            position = col_skip()
                           ), 
                           trim_ws = TRUE) %>%
  rename(first_name = vorname) %>%
  rename(gender = geschlecht) %>%
  distinct()

# let's make sure there are no double names

doubles_st <- german_names$first_name[duplicated(german_names$first_name)]

doubles_st <- german_names %>%
  filter(first_name %in% doubles_st) %>%
  distinct()

german_names <- german_names %>%
  anti_join(doubles_st)

remove(doubles_st)



# let's see then how it looks without those

corpus_token %>%
  anti_join(german_names %>%
              rename(token = first_name)) %>%
  filter(grepl("keller", doc_id)) %>% # let's look only at Keller
  group_by(title, token) %>%
  anti_join(stop_german, by = "token") %>% # delete stop words
  count() %>% # summarize count per token per title
  arrange(desc(n)) %>% # highest freq on top
  group_by(title) %>% # 
  mutate(top = seq_along(token)) %>% # identify rank within group # identify rank within group
  filter(top <= 15) %>% # retain top 15 frequent tokens
  # create barplot
  ggplot(aes(x = -top, fill = title)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure tokens are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = token), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "token count", x = "", # add labels
       title = "Keller: Most frequent tokens throughout the novels") +
  facet_grid(. ~ title) + # separate plot for each title
  coord_flip() + # flip axes
  scale_fill_sjplot()


# can you see any interesting pattern now?



# Sentiment analysis --------------

# "so, now, what about the sentiments?", you might ask?



## lexicons -----------

# as we mentioned in the first part of the tutorial, first we need to decide which lexicons we can use for sentiment analysis

# in this example, we will use only the sentiart lexicon.


## SentiART ----------

sentiart <- read.csv("SA_resources/SentiArt.dat",
                     dec = ",",
                     encoding = "UTF-8") %>%
  rename(token = 1) %>%
  mutate(ang_z = (ang_z - mean(ang_z))/sd(ang_z)) %>%
  select(-word)

head(sentiart)


corpus_token_SA <- corpus_token %>%
  left_join(sentiart)

remove(corpus, corpus_token, sentiart)

# in this case, we have performed an "left_join" function from the package tidyverse.
# this means that the combination of our corpus_token and the lexicons will only preserve the tokens for which a match exist, i.e. only tokens with a sentiment value.

# if you want to preserve the whole lexicon, with NA values for "empty" matches, you can use "full_join" instead. give it a try and see how it changes!


## let's have a look at our corpus_token

corpus_token_SA %>% head()



# we can have a look at the most frequent tokens with a sentiment value using a wordcloud. we can look at these all together, or select one specific sentiment for the wordcloud.

## wordclouds by sentiment ---------------

# frequent terms

corpus_token_SA %>%
  anti_join(stop_german, by = "token") %>% # delete stopwords
  group_by(token) %>%
  count() %>% # summarize count per token
  arrange(desc(n)) %>%
  head(50) %>%
  
  # mutate(log_n = sqrt(n)) %>% # take root to decrease outlier impact
  with(wordcloud(token, 
                 # log_n,
                 n,
                 max.tokens = 20, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
                 ))

# highest AAPz

corpus_token_SA %>%
  anti_join(stop_german, by = "token") %>% # delete stoptokens
  group_by(token) %>%
  summarise(n = mean(AAPz)) %>% # summarize mean AAPz per token
  arrange(desc(n)) %>%
  head(50) %>%

  with(wordcloud(token, 
                 n,
                 max.tokens = 50, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
                 scale = c(1, 3) 
  ))




# highest Happiness

corpus_token_SA %>%
  anti_join(stop_german, by = "token") %>% # delete stoptokens
  group_by(token) %>%
  summarise(n = mean(hap_z)) %>% # summarize mean AAPz per token
  arrange(desc(n)) %>%
  head(50) %>%
  
  with(wordcloud(token, 
                 n,
                 max.tokens = 50, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
                 scale = c(1, 3) 
  ))





# highest Anger

corpus_token_SA %>%
  anti_join(stop_german, by = "token") %>% # delete stoptokens
  group_by(token) %>%
  summarise(n = mean(ang_z)) %>% # summarize mean AAPz per token
  arrange(desc(n)) %>%
  head(50) %>%
  
  with(wordcloud(token, 
                 n,
                 max.tokens = 50, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
                 scale = c(1, 3) 
  ))



# sentiment across chapters --------

# another thing you might want to do is to have a look at how sentiment evolves acrosss a narrative.
# we can do so with some graphs:

# ANGER

corpus_token_SA %>%
  filter(grepl("keller", doc_id)) %>% # let's look only at Keller
  filter(!is.na(ang_z)) %>%
  group_by(title, sentence_id) %>%
  summarize(value = sum(ang_z, na.rm = T))  %>%
  # create area plot
  ggplot(aes(x = sentence_id, y = value, color=title)) +    
  geom_point() +
  # add black smoothed line without standard error
  geom_smooth(method = "loess", se = F) + 
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Sentence", y = "Sentiment value", # add labels
       title = "Sentiment across novels chapters") +
  # separate plot per title and dictionary and free up x-axes
  facet_grid(title ~ ., scale = "free_x") +
  scale_fill_sjplot()


# AAPz

corpus_token_SA %>%
  filter(grepl("keller", doc_id)) %>% # let's look only at Keller
  filter(!is.na(AAPz)) %>%
  mutate(value_type = ifelse(AAPz < 0, "negative", "positive")) %>%
  group_by(title, sentence_id, value_type) %>%
  summarize(value = sum(AAPz, na.rm = T))  %>%
  # create area plot
  ggplot(aes(x = sentence_id, y = value, color=value_type, fill=value_type)) +    
  geom_point() +
  # add black smoothed line without standard error
  geom_smooth(method = "loess", se = F) + 
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Sentence", y = "Sentiment value", # add labels
       title = "Sentiment across novels chapters") +
  # separate plot per title and dictionary and free up x-axes
  facet_grid(title ~ ., scale = "free_x") +
  scale_fill_sjplot()


 # let's see the plot

# we can also zoom in the plot

corpus_token_SA %>%
  filter(grepl("keller", doc_id)) %>% # let's look only at Keller
  filter(!is.na(AAPz)) %>%
  mutate(value_type = ifelse(AAPz < 0, "negative", "positive")) %>%
  group_by(title, sentence_id, value_type) %>%
  summarize(value = sum(AAPz, na.rm = T))  %>%
  # create area plot
  ggplot(aes(x = sentence_id, y = value, color=value_type, fill=value_type)) +    
  geom_point() +
  # add black smoothed line without standard error
  geom_smooth(method = "loess", se = F) + 
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Sentence", y = "Sentiment value", # add labels
       title = "Sentiment across novels chapters") +
  # separate plot per title and dictionary and free up x-axes
  facet_grid(title ~ ., scale = "free_x") +
  scale_fill_sjplot() + 
  coord_cartesian(ylim = c(-5,5)) 




# or have a look at the sentences with the highest values

## AAPZ

corpus_token_SA %>%
  group_by(author, title, sentence, sentence_id) %>%
  summarise(value = mean(AAPz, na.rm=T)) %>%
  arrange(desc(value)) %>%
  select(author, title, sentence, value) %>%
  head(10) %>%
  view()


# ANGER

corpus_token_SA %>%
  group_by(author, title, sentence, sentence_id) %>%
  summarise(value = mean(ang_z, na.rm=T)) %>%
  arrange(desc(value)) %>%
  select(author, title, sentence, value) %>%
  head(10) %>%
  view()

# HAPPINESS

corpus_token_SA %>%
  group_by(author, title, sentence, sentence_id) %>%
  summarise(value = mean(hap_z, na.rm=T)) %>%
  arrange(desc(value)) %>%
  select(author, title, sentence, value) %>%
  head(10) %>%
  view()
