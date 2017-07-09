library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(gutenbergr)

df <- read.csv("books.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#creating a new unique id var that is seperate from the gutenberg_id variable
id_table <- data.frame(unique(df$gutenberg_id)) %>%
  mutate(book_id = row_number()) %>%
  rename(gutenberg_id = unique.df.gutenberg_id.)

books <- df %>%
  select(gutenberg_id, text) %>%
  filter( text != "") %>%
  
  #saving a variable for the line number of each word
  group_by(gutenberg_id) %>%
  mutate(line = row_number()) %>%
  ungroup() %>%
  
  #matching unique id for the table from 'id_table', seperate from gutenberg_id
  full_join(id_table, by = "gutenberg_id") %>%
  select(book_id, gutenberg_id, line, text) %>%
  arrange(book_id) %>%
  
  #unnesting the individual words
  unnest_tokens(word, text) %>%
  
  #creating a new var for the word's position in it's book (identified with book_id)
  group_by(book_id) %>%
  mutate(posn = row_number()/n()) %>%
  mutate(posn_decile = ceiling(posn * 10)/10) %>%
  ungroup()

#finding the mean position of each word and its number of occurences from 'books'
words <- books %>%
  group_by(word) %>%
  summarize(avg_posn = mean(posn), n = n()) %>%
  filter(n > 1350) %>%
  arrange(desc(avg_posn))

head(words, 10)
tail(words, 10)

#boxplot of frequencies of 'words$n' (the count of number of instances of each word in 'books')
ggplot(data = words, aes(x = "", y = n)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 20))


  
  
