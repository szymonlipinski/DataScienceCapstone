library(knitr)
library(tidyverse)
library(tm)
library(dplyr)
library(wordcloud)
library(tidytext)
library(plyr)


basicDir <- "../data/final/en_US/"
twitterFilePath <- paste(basicDir, "en_US.twitter.txt", sep = "")
blogsFilePath <- paste(basicDir, "en_US.blogs.txt", sep = "")
newsFilePath <- paste(basicDir, "en_US.news.txt", sep = "")

twitterData <- readLines(twitterFilePath, encoding="UTF-8", skipNul = TRUE)
blogsData <- readLines(blogsFilePath, encoding="UTF-8", skipNul = TRUE)
newsData <- readLines(newsFilePath, encoding="UTF-8", skipNul = TRUE)


data <- list(twitterData, blogsData, newsData)
words <- sapply(data, function(x){lengths(strsplit(x, " "))})
twitterWords = lengths(strsplit(twitterData, " "))
blogsWords = lengths(strsplit(blogsData, " "))
newsWords = lengths(strsplit(newsData, " "))

rm(words)
rm(twitterWords)
rm(blogsWords)
rm(newsWords)
rm(twitterData)
rm(newsData)
rm(blogsData)
gc()

data <- unlist(data)
set.seed(1234)
sampleSize <- 10/100 # sample size in percents

indices <- function(){sample(seq_len(length(data)), length(data)*sampleSize)}
indices <- indices()
sampleData <- data[indices]

rm(data)
gc()

corpus <- Corpus(VectorSource(unlist(sampleData, use.names = FALSE)),
                 readerControl = list(reader = readPlain, language = "en"))


# Remove non-ASCII characters
corpus <- Corpus(VectorSource(sapply(corpus, function(row) iconv(row, "latin1", "ASCII", sub = ""))))

corpus <- corpus %>%
  tm_map(removePunctuation) %>% # Remove punctuation mar
  tm_map(removeNumbers) %>% # Remove number
  tm_map(stripWhitespace) %>% # Strip extra whitespace from a text document. Multiple whitespace characters are collapsed to asingle blank.
  tm_map(content_transformer(tolower)) %>% # make lower case
  tm_map(PlainTextDocument) # Create plain text documents.

corpusNoStopWords <- corpus %>%
  tm_map(removeWords, stopwords("english"))

plainText <- corpus[["content"]][["content"]]
plainTextNoStopWords <- corpusNoStopWords[["content"]][["content"]]

corpusDF <- data.frame(plainText)
colnames(corpusDF) <- c("text")

wordcloud(plainText, max.words = 200, random.order = FALSE, colors = brewer.pal(8,"Dark2"))

buildNGrams <- function(data, n) {
  data %>% 
    unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
    dplyr::count(ngram, sort = TRUE)
}


monoGrams <- buildNGrams(corpusDF, 1)
biGrams <- buildNGrams(corpusDF, 2)
triGrams <- buildNGrams(corpusDF, 3)
quadGrams <- buildNGrams(corpusDF, 4)
 
