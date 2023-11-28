
# Module 1: Introduction to Sentiment Analysis

# Introduction to lexicon-based sentiment analysis
# Lexicon-based sentiment analysis involves using predefined dictionaries (lexicons)
# that associate words with sentiment scores. This approach is useful for analysing
# sentiment in text data without the need for training a machine learning model.

# Section 1: Preparing Your Corpus

# Install and load required packages
# install.packages(c("gutenbergr", "tidyverse"))
library(gutenbergr)
library(tidyverse)

# https://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/GUTINDEX.ALL


# Create a Corpus of Novels from a Specific Category

# Let's have a look at the categories of books in Gutenberg by 'shelf'
gutenberg_metadata %>% group_by(gutenberg_bookshelf) %>% count() %>% arrange(desc(n))

# Set the working directory to the folder where you want to save the corpus
# setwd("/path/to/your/directory")
getwd()

# Get novels from Gutenberg for the chosen category
corpus <- gutenberg_works(gutenberg_bookshelf == "Children's Book Series") %>%
    filter(has_text == T, language == "en") %>%
    slice_sample(n = 30) %>%
    gutenberg_download(meta_fields = c("title", "author")) %>%
    group_by(title, author, gutenberg_id) %>%
    dplyr::summarise(text=paste(text, collapse=" "))

# remove extra spaces
corpus <- corpus %>%
  mutate(text = stringr::str_trim(text))

# Write texts in 'corpus' folder as single txtv files
# walk2(corpus$title, corpus$text, ~ writeLines(.y, file.path("corpus", paste0(.x, ".txt"))))

# Alternatively, you can load the corpus from the 'corpus' folder
  # corpus_path <- "corpus"
  # file_list <- list.files(path = corpus_path, pattern = ".txt", full.names = T, recursive = F)

  # Read text data from each file and combine into a list
  # corpus2 <- readtext::readtext(file_list)

# Section 2: Sentiment Lexicons and Preprocessing

# install.packages("syuzhet")
library(syuzhet)
# install.packages("tidytext")
library(tidytext)

# Select a lexicon
# Uncomment and choose the desired lexicon

lexicon_afinn <- get_sentiment_dictionary("afinn")
lexicon_bing <- get_sentiment_dictionary("bing")
lexicon_nrc <- get_sentiment_dictionary("nrc")

stopwords <- stop_words %>%
  select(word)

# Preprocess text data
corpus_df <- corpus %>%
  unnest_tokens(input = text, output = word, to_lower = T, drop = T) %>%
  anti_join(stopwords)


# Module 4: Sentiment Analysis Coding Exercise

# Perform sentiment analysis
# Uncomment and complete the code
sentiment_scores <- corpus_df %>%
  inner_join(lexicon_afinn, by = "word") %>%
  group_by(title) %>%
  summarize(sentiment_score = sum(value))

# Module 5: Visualization of Sentiment Analysis Results

# Visualize sentiment scores
# Uncomment and complete the code
ggplot(sentiment_scores, aes(x = title, y = sentiment_score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Sentiment Analysis Results", x = "Title", y = "Sentiment Score") +
  theme_minimal()

# Module 6: Conclusion and Next Steps

# Wrap-up and summary
cat("Sentiment analysis with R course completed.\n")

# Next steps and resources
cat("Next steps:\n")
cat("- Explore advanced sentiment analysis techniques.\n")
cat("- Dive deeper into text mining and natural language processing.\n")
cat("- Stay updated with the latest developments in R and sentiment analysis.\n")

# Save visualizations or export sentiment scores as needed
# Uncomment and modify as needed
# ggsave("sentiment_analysis_visualization.png", plot = last_plot())
# write.csv(sentiment_scores, "sentiment_scores.csv", row.names = FALSE)
