
library(tidyverse)
library(readxl)
library(tidytext)
library(readtext)
library(sjPlot)

load("corpus.RData")

stopwords_en <- stop_words %>%
  as_tibble() %>%
  rename(token = word) %>%
  select(token)


# what if we wanteed to see how gender plays a role in our coprus?

# gender applies here to two aspects at least: author gender and represented gender.
# how can we investigate these?

# first we need our corpus - run the crpus creation part of script 02 to recreate it

# we also need a list of proper names, which contains also gender information about
# can you find one?

library(babynames)

names_en <- babynames %>%
  as_tibble() %>%
  select(name, sex) %>%
  mutate(name = tolower(name)) %>%
  rename(gender = sex) %>%
  anti_join(stopwords_en, by=c("name"="token")) %>%
  distinct()

corpus_token <- corpus_token %>%
  left_join(names_en, by=c("token"="name"))

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

# now we can apply the names to our corpus_1, obtaining

corpus_token_SA <- corpus_token_SA %>%
  left_join(
    german_names, by = "first_name"
  ) %>%
  rename(author_gender = gender)


# we can see how much of our corpus_1 has been written by authors of different genres

corpus_token_SA_total <- corpus_token_SA %>%
  select(doc_id, sentence_id, token_id) %>%
  distinct() %>%
  nrow()

female_total <- corpus_token_SA %>%
  select(doc_id, sentence_id, author_gender, token_id) %>%
  distinct() %>%
  filter(author_gender == "w") %>%
  nrow()

male_total <- corpus_token_SA %>%
  select(doc_id, sentence_id, author_gender, token_id) %>%
  distinct() %>%
  filter(author_gender == "m") %>%
  nrow()



# proportion of author_gender by author (how many authors x gender)

corpus_token_SA %>%
  select(author, author_gender) %>%
  distinct() %>%
  group_by(author_gender) %>%
  count() %>%
  ggplot(aes(y=n, x=author_gender, fill=author_gender, label=n)) +
  geom_col() +
  geom_text(nudge_y = 2)

# over time

corpus_token_SA %>%
  select(author, author_gender, pub_date) %>%
  distinct() %>%
  group_by(author_gender, pub_date) %>%
  count() %>%
  ggplot(aes(y=n, x=pub_date, color=author_gender)) +
  geom_smooth()


#
###  corpus_1 long -----

corpus_aggr_long <- corpus_token_SA %>%
  select(collection,
         author,
         author_gender,
         title,
         doc_id,
         pub_date,
         sentence_id,
         sentence, 
         AAPz,
         fear_z,
         disg_z,
         hap_z,
         sad_z,
         surp_z,
         ang_z)  %>%
  dplyr::group_by(collection, 
                  author,
                  author_gender,
                  title,
                  doc_id, 
                  sentence_id,
                  sentence,
                  pub_date) %>%
  dplyr::summarise(words_sent = n(),
                   Sentiart_AAPz_mean = ifelse(!is.nan(mean(AAPz, na.rm = T)), mean(AAPz, na.rm = T), NA),
                   Sentiart_fear_z_mean = ifelse(!is.nan(mean(fear_z, na.rm = T)), mean(fear_z, na.rm = T), NA),
                   Sentiart_disg_z_mean = ifelse(!is.nan(mean(disg_z, na.rm = T)), mean(disg_z, na.rm = T), NA),
                   Sentiart_hap_z_mean = ifelse(!is.nan(mean(hap_z, na.rm = T)), mean(hap_z, na.rm = T), NA),
                   Sentiart_sad_z_mean = ifelse(!is.nan(mean(sad_z, na.rm = T)), mean(sad_z, na.rm = T), NA),
                   Sentiart_surp_z_mean = ifelse(!is.nan(mean(surp_z, na.rm = T)), mean(surp_z, na.rm = T), NA),
                   Sentiart_ang_z_mean = ifelse(!is.nan(mean(ang_z, na.rm = T)), mean(ang_z, na.rm = T), NA),
  ) %>%
  select(-words_sent)  %>%
  pivot_longer(c("Sentiart_AAPz_mean",
               "Sentiart_fear_z_mean",
               "Sentiart_disg_z_mean",
               "Sentiart_hap_z_mean",
               "Sentiart_sad_z_mean",
               "Sentiart_surp_z_mean",
               "Sentiart_ang_z_mean"), names_to = "sentiment", values_to = "sentiment_value" )


# ------------

# we can also see how author gender play a role in the sentiment that takes place in the corpus_1

## mean sentiment in corpus_1 by author gender

corpus_aggr_long %>%
  group_by(author_gender, sentiment) %>%
  summarise(sentiment_value = mean(sentiment_value, na.rm=T)) %>%
  group_by(sentiment, author_gender) %>%
  filter(!is.na(sentiment)) %>%
  ggplot(aes(x=sentiment, y=sentiment_value, fill=author_gender)) +
  geom_col(position = "dodge", stats="identity")



# mean sentiment value per year by gender

corpus_aggr_long %>%
  group_by(sentiment, author_gender, pub_date) %>%
  summarise(sentiment_value = mean(sentiment_value, na.rm=T)) %>%
  ggplot(aes(y=sentiment_value, x=pub_date, color=author_gender)) +
  geom_smooth()

corpus_aggr_long %>%
  group_by(sentiment, author_gender, pub_date) %>%
  summarise(sentiment_value = mean(sentiment_value, na.rm=T)) %>%
  ggplot(aes(y=sentiment_value, x=pub_date, color=author_gender)) +
  geom_point()


# we might also be more interested in the represented gender, so how can we analyse it?

# again, we neeed to be able to identify names or words that are identificative of gender.
# we decided here to look at the list of proper names, as well as words that are stereotipically considered as representative of a gender (apologies for the binary structure, for the purpose of this worshop we will have to limit to female/male dualism here.)

# represented gender -----------------


## proper names index -----------

### FEMALE corpus_1 --------

corpus_gender_female <- corpus_token_SA %>%
  select(author,
         title,
         doc_id,
         sentence,
         sentence_id,
         token,
         token_id) %>%
  distinct() %>%
  left_join(german_names %>%
              filter(gender == "w") %>%
              anti_join(stop_german, by = c("first_name" = "token")) %>%
              rename(name_gender = gender),
            by = c("token" = "first_name")) %>%
  filter(grepl("Frau", token) |
           grepl("Mutter", token) |
           # token == "sie" |
           grepl("Schwester", token) |
           grepl("Tante", token) |
           grepl("Mädchen", token) |
           grepl("Dame", token) |
           grepl("Tochter", token) |
           grepl("Lehrerin", token) |
           grepl("rerin", token) |
           grepl("Fräulein", token) |
           name_gender == "w") %>%
  mutate(is_gender_word = 1)  %>%
  mutate(gender_type = "female") %>%
  dplyr::group_by(author,
                  title,
                  doc_id,
                  sentence_id,
  ) %>%
  summarise(gender_words_n = sum(is_gender_word, na.rm=T),
            gender_item = paste0(list(token[!is.na(token)])),
            gender_type = "female",
            is_gender_word = 1)


# MALE corpus_1

corpus_gender_male <- corpus_token_SA %>%
  select(author,
         title,
         doc_id,
         sentence,
         sentence_id,
         token,
         token_id) %>%
  distinct() %>%
  left_join(german_names %>%
              filter(gender == "m") %>%
              anti_join(stop_german, by = c("first_name" = "token")) %>%
              rename(name_gender = gender),
            by = c("token" = "first_name")) %>%
  filter(grepl("Herr", token) |
           grepl("Vater", token) |
           token == "er" |
           grepl("Bruder", token) |
           grepl("Onkel", token) |
           grepl("Ritter", token) |
           grepl("Sohn", token) |
           token == "Lehrer" |
           name_gender == "m") %>%
  mutate(is_gender_word = 1)  %>%
  mutate(gender_type = "male") %>%
  dplyr::group_by(author,
                  title,
                  doc_id,
                  sentence_id,
  ) %>%
  summarise(gender_words_n = sum(is_gender_word, na.rm=T),
            gender_item = paste0(list(token[!is.na(token)])),
            gender_type = "male",
            is_gender_word = 1)

# GENDER CORPUS

corpus_gender <- bind_rows(corpus_gender_female, corpus_gender_male)
remove(corpus_gender_female, corpus_gender_male)

corpus_aggr_long <- corpus_aggr_long %>%
  left_join(corpus_gender)


remove(german_names)

# mean sentiment by represented gender (Sentiart)

library(table1)

corpus_aggr_long %>%
  group_by(gender_type, sentiment) %>%
  summarise(sentiment_value = mean(sentiment_value, na.rm=T)) %>%
  ggplot(aes(y=sentiment_value, x=gender_type, fill=gender_type, label=round(sentiment_value, 3))) +
  geom_col(position="dodge") +
  geom_text(nudge_y = -.04) +
  facet_wrap(. ~ sentiment) +
  ggtitle("Mean sentiment values in sentences with gendered words")




# schematic representations

table1::table1(~ sentiment_value | sentiment + gender_type, 
               data=corpus_aggr_long, overall=F)

table1::table1(~ sentiment_value | sentiment + author_gender, 
               data=corpus_aggr_long, overall=F)

# and by author

corpus_aggr_long %>%
  filter(!is.na(gender_type)) %>%
  group_by(author_gender, gender_type, sentiment) %>%
  summarise(sentiment_value = mean(sentiment_value, na.rm=T)) %>%
  ggplot(aes(y=sentiment_value, x=author_gender, fill=author_gender, 
             label=round(sentiment_value, 3))) +
  geom_col(position="dodge") +
  geom_text() +
  facet_wrap(gender_type ~ sentiment) +
  ggtitle("Mean sentiment values in sentences by author gender")



# keywords

library(quanteda)

toks <- corpus_aggr_long %>%
  select(sentence, sentence_id) %>%
  distinct()

toks <- toks$sentence

toks <- quanteda::corpus(toks)

quanteda::kwic(tokens(toks), pattern = "Frau")

quanteda::kwic(tokens(toks), pattern = "Mann")


head(tokens_ngrams(tokens(paste0(frau_kwic$pre, frau_kwic$post), remove_punct = TRUE)), 30)



corpus_aggr_long %>%
  filter(!is.na(gender_type)) %>%
  group_by(title, sentiment, gender_type) %>%
  summarise(gender_words_n = sum(gender_words_n),
  sentiment_value = sum(sentiment_value)) %>%
  ggplot((aes(x=gender_words_n, y=sentiment_value, color=gender_type))) +
  geom_point() +
  geom_smooth(color="black") +
  scale_fill_sjplot() +
  facet_grid(sentiment ~ gender_type, scales = "free_x")



corpus_aggr_long %>%
  filter(!is.na(gender_type)) %>%
  group_by(title, sentiment, gender_type) %>%
  summarise(gender_words_n = sum(gender_words_n),
            sentiment_value = sum(sentiment_value)) %>%
  ggplot((aes(x=gender_words_n, y=sentiment_value, color=gender_type))) +
  geom_point() +
  geom_smooth(color="black") +
  scale_fill_sjplot() +
  facet_grid(sentiment ~ gender_type, scales = "free_x")

