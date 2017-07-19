library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(gutenbergr)
library(data.table)





#---------------------#
##### EXPLORATION #####
#---------------------#

#### ...Attempt 1 ####

#pull all book metadata that is english and has text (can be downloaded from project gutenberg)
temp_1 <- gutenberg_metadata %>%
  filter(language == "en", has_text == TRUE)

#first attempt at finding a suitable bookshelf directed me to the "Bestsellers, American, 1895-1923" bookshelf
#A collection of just under 100 works form the time period. However this led to less than ideal results with much
#text being housekeeping items and not enough actual text

#find bookshelfs with at least 100 books and arrange in descending order
temp_2 <- data.frame(table(temp_1$gutenberg_bookshelf)) %>%
  filter(Freq > 100) %>%
  arrange(desc(Freq))

download_list <- temp_1 %>%
  filter(gutenberg_bookshelf == "Bestsellers, American, 1895-1923") %>%
  filter(author != "Churchill, Winston") %>%
  filter(author != "Parker, Gilbert") %>%
  select(gutenberg_id)

books <- gutenberg_download(download_list, strip = TRUE)

write.csv(books, file = "books.csv")



#### ...Attempt 2 ####

temp_1 <- gutenberg_metadata %>%
  filter(language == "en", has_text == TRUE)

#903 unique bookshelves but some are sub-categories of others, deliminated by "/"
length(unique(temp_1$gutenberg_bookshelf))
#Attempt to extract base bookshelves only by removing all text after a "/"
temp_2 <- temp_1 %>%
  mutate(bookshelf = gsub("/\\S*", "", gutenberg_bookshelf))
#Only 588 bookshelfs now but 3/4 are NA's (30704/41452)
length(unique(temp_2$bookshelf))
#Let's see what has a lot of books
temp_3 <- data.frame(table(temp_2$bookshelf)) %>%
  arrange(desc(Freq))
head(temp_3, 50)

#reveiwing the list of the top 50 by number of books, lets go with the following bookshelves
#Any fiction, Bestsellers, American, 1895-1923, Best Books Ever Listings, and Harvard Classics

#filter for the above bookshelves, select the id column only (to pass to gutenberg_download), and group_by to remove
# duplicates if any (spoiler: there weren't)
temp_4 <- temp_2 %>%
  filter(grepl("Fiction|fiction|Bestsellers, American, 1895-1923|Best Books Ever Listings|Harvard Classics", bookshelf)) %>%
  select(gutenberg_id) %>%
  group_by(gutenberg_id) %>%
  filter(!(gutenberg_id %in% c(19506, 19513, 19515, 19574, 19796)))

#download the books from project gutenberg, strip = TRUE is a function that attempts to strip out headers and foots
books <- gutenberg_download(temp_4, strip = TRUE)

#write the file as a csv to the wd
write.csv(books, file = "books.csv")



#----------------------#
###### PREPARATION #####
#----------------------#

#### ...Preparing Data ####

df <- fread("books.csv", sep = ",", header = TRUE)
#turns out 16 million lines of text may be a little much for later analysis with my 8gb of RAM
#lets cut it in about half down to a 5% sample of book id's
books <- sample(unique(df$gutenberg_id), 0.1*length(unique(df$gutenberg_id)), replace = FALSE) %>%
  data.frame() %>%
  rename(gutenberg_id = ".")
books <- left_join(books, df) %>%
  select(gutenberg_id, text)


books <- books %>%
  select(gutenberg_id, text) %>%
  filter(text != "") %>%
  
  #saving a variable for the line number of each word
  group_by(gutenberg_id) %>%
  mutate(line = row_number()) %>%
  ungroup() %>%
  arrange(gutenberg_id) %>%
  
  #unnesting the individual words
  unnest_tokens(word_raw, text) %>%
  
  #creating a new var for the word's position in it's book (identified with book_id)
  group_by(gutenberg_id) %>%
  mutate(posn = row_number()/n()) %>%
  mutate(posn_decile = ntile(posn, n = 10)) %>%
  ungroup() 

#getting rid of the many underscores ("_") that seem to have replaced spaces in the text
#doesn't seem to work in the orignal call to create 'books' for unknown reason
books_2 <- books %>%
  mutate(word = gsub("_", "", books$word_raw)) %>%
  select(-word_raw)
books <- books_2
rm(books_2)
rm(df)
gc()

#getting total word frequencies
words <- books %>%
  group_by(word) %>%
  summarise(freq = n()) %>%
  data.frame() %>%
  ungroup %>%
  arrange(desc(freq))

#getting total word decile frequencies (number of deciles the word appears in)
words_decile <- books %>%
  select(word, posn_decile) %>%
  group_by(word, posn_decile) %>%
  filter(row_number() == 1) %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  data.frame() %>%
  ungroup() %>%
  arrange(desc(n))

#### ...Preparing AFINN ####

#let's pull the afinn lexicon of sentiments from the 'sentiments' dataset that comes with tidytext
afinn <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, sentiment = score)

#and now let's add the sentiment to the books dataset
books <- books %>%
  left_join(afinn, by = "word") %>%
  arrange(gutenberg_id)



#-----------------#
##### SANDBOX #####
#-----------------#


#setting up words to be graphed on their frequency over ten deciles
words_decile <- books %>%
  group_by(word, posn_decile) %>%
  summarise(freq = n()) %>%
  data.frame() %>%
  ungroup %>%
  arrange(desc(freq))
words <- books %>%
  group_by(word) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  filter(freq > 1000)
words_data <- words_decile %>%
  group_by(word) %>%
  mutate(z = (freq - min(freq))/(max(freq)-min(freq))) %>%
  summarise(min = min(freq), 
            median = median(freq), 
            max = max(freq),
            sd = sd(z),
            freq = n()) %>%
  inner_join(words, by = "word") %>%
  arrange(desc(sd))

words_temp <- words_decile[words_decile$word == "change",]

ggplot(data = words_temp, aes(x = posn_decile, y = freq)) +
       geom_histogram(stat = "identity")




#setting up graphing of sentiment across ten deciles for all books together
books_decile_sentiment <- books %>%
  group_by(posn_decile) %>%
  summarise(avg_sentiment = mean(sentiment, na.rm = TRUE))
ggplot(books_decile_sentiment, aes(x = posn_decile, y = avg_sentiment)) +
  geom_histogram(stat = "identity")







#create a list of words used in each decile, words used in multiple deciles will duplicate
words <- data.frame()
for (i in 1:10) {
  words_decile_temp <- books %>%
    filter(posn_decile == i) %>%
    select(word) %>%
    table() %>%
    data.frame() %>%
    arrange(desc(Freq))
  words <- rbind(words, words_decile_temp)
}
colnames(words) <- c("word", "Freq")
words$word <- as.character(words$word)
rm(words_decile_temp)

#count use of words accross deciles
word_count <- words %>%
  group_by(word) %>%
  summarise( Freq = n())

ggplot(data = words_count, aes(x = "", y = Freq)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 8))





#finding the mean position of each word and its number of occurences from 'books'
words <- books %>%
  group_by(word) %>%
  summarize(avg_posn = mean(posn), n = n()) %>%
  filter(n > 100) %>%
  arrange(desc(avg_posn))

head(words, 20)
tail(words, 20)





#boxplot of frequencies of 'words$n' (the count of number of instances of each word in 'books')
ggplot(data = words, aes(x = "", y = n)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1500))

temp_1 <- data.frame()
for (i in 1:10) {
  temp_1[i,1] <- nrow(books[books$word == "the" & books$posn_decile == 0 + 0.1*i,])
}
