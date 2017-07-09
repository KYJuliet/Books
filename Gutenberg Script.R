library(dplyr)
library(tidyr)
library(gutenbergr)

#pull all book metadata that is english and has text (can be downloaded from project gutenberg)
temp_1 <- gutenberg_metadata %>%
  filter(language == "en", has_text == TRUE)

#find bookshelfs with at least 100 books and arrange in descending order
temp_2 <- data.frame(table(temp_1$gutenberg_bookshelf)) %>%
  filter(Freq > 100) %>%
  arrange(desc(Freq))

###

download_list <- temp_1 %>%
  filter(gutenberg_bookshelf == "Bestsellers, American, 1895-1923") %>%
  filter(author != "Churchill, Winston") %>%
  filter(author != "Parker, Gilbert") %>%
  select(gutenberg_id)
  
books <- gutenberg_download(download_list, strip = TRUE)

write.csv(books, file = "books.csv")