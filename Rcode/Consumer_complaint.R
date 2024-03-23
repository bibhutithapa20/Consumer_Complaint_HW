library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(shiny)
library(ggplot2)
library(DT)
library(sentimentr)
library(wordcloud)
library(tidytext)
library(textdata)
library(RColorBrewer)
library(reshape2)

rm(list=ls())

setwd('~/Documents/Data_Consumer_Complaints')

data <- read.csv('Consumer_Complaints.csv')

my_data <- data %>%
  select(Date.received, Product, Issue, Company, State, Submitted.via, Company.response.to.consumer, Timely.response., Consumer.disputed., Complaint.ID) %>%
  rename(Date_received = Date.received) %>%
  rename(Company_response = Company.response.to.consumer) %>%
  rename(Submitted_via = Submitted.via) %>%
  rename(Timely_Response = Timely.response.) %>%
  rename(Consumer_disputed = Consumer.disputed.) %>%
  rename(Consumer_ID = Complaint.ID)

my_data$Issue <- tolower(my_data$Issue)

# Select only the first 50,000 rows of data for analysis
my_data_50 <- my_data[1:50000, ]
my_data_50$Issue <- tolower(my_data_50$Issue)

data_df <- unnest_tokens(tbl = my_data_50, input = Issue, output = word) # Corrected to use my_data_50 instead of my_data

stop_words <- get_stopwords(source = "smart")
data_df <- anti_join(data_df, stop_words, by = "word")

bing_table <- data_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, Product, Company, State, Submitted_via, Consumer_disputed, Date_received, sentiment, sort = TRUE) %>%
  ungroup()

bing_table <- spread(key = sentiment, value = n, fill = 0, data = bing_table)
bing_table <- mutate(bing_table, sentiment = positive - negative)

mean_sentiment <- mean(bing_table$sentiment, na.rm = TRUE)

# Data Visualization: Bar chart of sentiment scores by submission method
ggplot(aes(x = Submitted_via, y = sentiment, fill = Submitted_via), data = bing_table) +
  geom_col(show.legend = TRUE) +
  facet_wrap(vars(), ncol = 1, scales = "free_x") +
  labs(x = "Submitted Via", y = "Sentiment") +
  theme_classic()

# Data Visualization: Bar chart of sentiment scores by word
ggplot(aes(x = word, y = sentiment, fill = State), data = bing_table) +
  geom_col(show.legend = TRUE) +
  facet_wrap(vars(), ncol = 1, scales = "free_x") +
  labs(x = "Word", y = "Sentiment") +
  theme_classic()

# Data Visualization: Bar chart of sentiment scores by product
ggplot(bing_table, aes(word, sentiment, fill = Product)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Product, ncol = 2, scales = "free_x")

# Create a word cloud of negative words
set.seed(1234) # for reproducibility 
wordcloud(words = bing_table$word, freq = bing_table$negative, scale = c(4.0,0.75), 
          max.words = 200, colors = brewer.pal(7, "Dark2"))

# Create a word cloud of positive words
set.seed(1234) # for reproducibility 
wordcloud(words = bing_table$word, freq = bing_table$positive, scale = c(3.5,0.95), 
          max.words = 200, colors = brewer.pal(3, "Dark2"))
