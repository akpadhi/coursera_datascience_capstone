


library(stringi)
library(RWekajars)
library(ggplot2)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(qdap)
library(NLP)
library(tm)
library(SnowballC)
library(slam)
library(RWeka)
library(rJava)


setwd("/Users/akshayapadhi/Documents/myStuff/courses/10_dataScienceCapstone/CourseraCapstoneFinal")

#### Getting Data
file_conn = file("./final/en_US/en_US.blogs.txt")
blogs <- readLines(file_conn, encoding ="UTF-8", skipNul=TRUE)
#blogs <- iconv(blogs, to = "utf-8-mac")
close(file_conn)

file_conn = file("./final/en_US/en_US.news.txt")
news <- readLines(file_conn, encoding ="UTF-8", skipNul=TRUE)
#news <- iconv(news, to = "utf-8-mac")
close(file_conn)

file_conn = file("./final/en_US/en_US.twitter.txt")
twitter <- readLines(file_conn, encoding ="UTF-8", skipNul=TRUE)
#twitter <- iconv(twitter, to = "utf-8-mac")
close(file_conn)


###Data Statistics

stats <- data.frame(File_Name=c("US_blogs", "US_news", "US_twitter"), 
										FileSize=c(file.info("./final/en_US/en_US.blogs.txt")$size/1024*1024, file.info("./final/en_US/en_US.news.txt")$size/1024*1024, file.info("./final/en_US/en_US.twitter.txt")$size/1024*1024),
										WordCount=sapply(list(blogs, news, twitter), stri_stats_latex)[4,], 
										t(rbind(sapply(list(blogs, news, twitter), stri_stats_general)[c('Lines','Chars'),]
										)))
head(stats)


#### sampling

# Data is very big. Sample 1% of data for easy exploratory data analysis. Use TM package to clean the data. 
# - create the corpus
# - covert lower case
# - removing white spaces, punctuation, stop words, numbers, strip white space, etc.
# - remove bad words (google bad words)


# sample_data1 <- c(sample(blogs, 2000),
#  								 sample(news, 2000),
#  								 sample(twitter, 2000))

sample_data <- c(sample(blogs, length(blogs)/50),
								 sample(news, length(news)/100),
								 sample(twitter, length(twitter)/100))


# writeLines(sample_data1, "./dataFiles/sample_data1.txt")
writeLines(sample_data, "./dataFiles/sample_data.txt")
# sample_data <- c(sample(blogs, length(blogs)/100))

rm(list = ls())
#####Create and clean corpus
## Building a clean corpus


sample_data <- readLines("./dataFiles/sample_data.txt")

##Bad words list from Google
badWords <- read.table("./google_bad_words.txt", header = FALSE, fill = TRUE)

## Build the corpus, and specify the source to be character vectors 
corpus <- VCorpus(VectorSource(sample_data))

##
rm(sample_data)

## Make it work with the new tm package
corpus <- tm_map(corpus,
											content_transformer(function(x) 
												iconv(x, to="UTF-8", sub="byte")))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)

## Convert to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

## remove punction, numbers, URLs, stop, profanity and stem wordson

corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+") # remove URLs
# corpus <- tm_map(corpus, toSpace, "@[^\\s]+") # replace twitter account w space
corpus <- tm_map(corpus, toSpace, "RT |via ") # remove retweets and vias
corpus <- tm_map(corpus, removeSpecialChars) # remove everything that is not alphanumerical symbol or space
corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, toSpace, "[^[:alnum:]]")
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, badWords[,1])
# corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, PlainTextDocument)

corpus <- VCorpus(VectorSource(corpus))

## Saving the final corpus
saveRDS(corpus, file = "./dataFiles/cleanedCorpus.RData")
rm(list = ls())



##### Read corpus and convert to DF

corpus<-readRDS("./dataFiles/cleanedCorpus.RData")
corpusDF <-data.frame(text=unlist(sapply(corpus,`[`, "content")), 
											stringsAsFactors = FALSE)

ngramTokenizer <- function(corpus, ngramCount) {
	ngram <- NGramTokenizer(corpus, 
																	Weka_control(min = ngramCount, max = ngramCount, 
																							 delimiters = " \\r\\n\\t.,;:\"()?!"))
	ngram
}

####Tokenizing
library(dplyr)           
library(tidyr)

#### Unigrams

unigrams <- ngramTokenizer(corpusDF, 1)

unigrams_sorted<-data.frame(table(unigrams)) %>%
	rename(words = unigrams, count = Freq) %>%
	arrange(-(count)) %>%
	mutate(words = as.character(words)) %>%
	mutate(word1 = words) %>%
	select(word1, count)

head(unigrams_sorted)

saveRDS(unigrams_sorted, file = "./dataFiles/unigrams.RData")

#### Bigrams
bigrams <- ngramTokenizer(corpusDF, 2)

bigrams_sorted<-data.frame(table(bigrams)) %>%
	rename(words = bigrams, count = Freq) %>%
	arrange(-(count)) %>%
	mutate(words = as.character(words)) %>%
	separate(words, c("word1", "word2"), sep = " ")

# names(bigrams_sorted)
head(bigrams_sorted)

saveRDS(bigrams_sorted, file = "./dataFiles/bigrams.RData")

#### Trigrams
trigrams <- ngramTokenizer(corpusDF, 3)

trigrams_sorted<-data.frame(table(trigrams)) %>%
	rename(words = trigrams, count = Freq) %>%
	arrange(-(count)) %>%
	mutate(words = as.character(words)) %>%
	separate(words, c("word1", "word2", "word3"), sep = " ")

# names(trigrams_sorted)
head(trigrams_sorted)

saveRDS(trigrams_sorted, file = "./dataFiles/trigrams.RData")

#### Quadgrams
quadgrams <- ngramTokenizer(corpusDF, 4)

quadgrams_sorted<-data.frame(table(quadgrams)) %>%
	rename(words = quadgrams, count = Freq) %>%
	arrange(-(count)) %>%
	mutate(words = as.character(words)) %>%
	separate(words, c("word1", "word2", "word3", "word4"), sep = " ")

# names(quadgrams_sorted)
head(quadgrams_sorted)

saveRDS(quadgrams_sorted, file = "./dataFiles/quadgrams.RData")

