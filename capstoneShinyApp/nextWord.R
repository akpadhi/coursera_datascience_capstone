library(NLP)
library(tm)
library(stringr)
library(markdown)
library(stylo)


unigram <- readRDS(file="./dataFiles/unigrams.RData")
bigram <- readRDS(file="./dataFiles/bigrams.RData")
trigram <- readRDS(file="./dataFiles/trigrams.RData")
quadgram <- readRDS(file="./dataFiles/quadgrams.RData")

cleanInput <- function(text){
	
	cleanText <- tolower(text)
	cleanText <- removePunctuation(cleanText)
	cleanText <- removeNumbers(cleanText)
	cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
	cleanText <- stripWhitespace(cleanText)
	
	cleanText <- txt.to.words.ext(cleanText, language="English.all",preserve.case = TRUE)
	return(cleanText)
}


predictNextWord <- function(text){
	
	textInput <-cleanInput(text)
	
	wordCount <- length(textInput)
	if (wordCount>=3) {
		textInput <- textInput[(wordCount-2):wordCount]
	}
	else if(wordCount==2) {
		textInput <- c("",textInput)
	}
	else {
		textInput <- c("","",textInput)
	}
	
	nextWord <- as.character(quadgram[quadgram$word1 == textInput[1] &
																			quadgram$word2 == textInput[2] &
																			quadgram$word3 == textInput[3],][1,]$word4)
	
	if(is.na(nextWord)) {
		nextWord <- as.character(trigram[trigram$word1==textInput[2] &
																		 	trigram$word2==textInput[3],][1,]$word3)
		
		if(is.na(nextWord)) {
			nextWord <- as.character(bigram[bigram$word1==textInput[3],][1,]$word2)
			
			
			if(is.na(nextWord)) {
				nextWord <- as.character(unigram[grep(paste("^",textInput[3],sep=""),unigram$word1),][1,]$word1)
			}
		}
	}
	if(is.na(nextWord)) {cat(paste(" No word suggestion"))}
	else { cat(paste(nextWord))}
}	
