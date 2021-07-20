####### Author: Matheus Dias I. Santos, Economist ######
#######  Project under development ####


library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(tm)
library(wordcloud)



##########Tokens Here###############
app_name <- 'YOUR APP NAME'
b_token <- 'YOUR TOKEN'
access_token <-  'YOUR ACESS TOKEN'
access_secret <- 'YOUR SECRET TOKEN'
consumer_key <- 'YOUR CONSUMER KEY'
consumer_secret <- 'YOUR SECRET KEY'



###########################################
##########Create Token#####################

create_token(app=app_name,
             consumer_key = consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token,
             access_secret =access_secret)



#########################################################
###########Lists of stocks###############################

word_key <- "PETR4"
#########################################################
tweets_output <- search_tweets(word_key, n = 10000, include_rts = FALSE)

tweets_result = tweets_output %>% select(screen_name, text)

###### Converting twitter texts to Corpus format###########

my_corpus <- Corpus(VectorSource(tweets_result$text))


#############Convert the text to lower######
my_corpus <- tm_map(my_corpus, content_transformer(tolower))

#####Remove punctuation###########################

my_corpus <- tm_map(my_corpus, removePunctuation)

######Remove Numbers#####################
####OBS:Must be assessed case by case####

#  my_corpus <- tm_map(my_corpus, removeNumbers)

#######Remove URLs############

remove_URL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)

my_corpus <- tm_map(my_corpus, content_transformer(remove_URL))

####Stopwords are irrelevant words e.g: pra, vai, ?, pois,...######

my_stopwords <- c(stopwords("portuguese"),c("ai","?", "pois","kkk","kkkk","ibov","r","q","ibovespa"))

#####Use this line in case you want to analyse especific words that are stop words###

#  myStopwords <- setdiff(my_stopwords, c("?", "r"))

###### remove stopwords from corpus

my_corpus <- tm_map(my_corpus, removeWords, my_stopwords)


## keep a copy of corpus to use later as a dictionary for stem
# completion

my_corpus_copy <- my_corpus

# stem words
my_corpus <- tm_map(my_corpus, stemDocument)

####Code to inspect the texts
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(my_corpus[[i]])
  writeLines(as.character(my_corpus[[i]]))
}

###########StemCompletion this line complete some words heuristicaly based on the dictionary######
######e.g. the word compan could be heuristicaly completed as company#######



#my_corpus <- tm_map(my_corpus, content_transformer(stemCompletion), dictionary = my_corpus_copy, lazy=TRUE)


tdm <- TermDocumentMatrix(my_corpus, control = list(wordLengths = c(1, Inf)))
tdm

## Freqency words and Association


idx <- which(dimnames(tdm)$Terms == word_key)
inspect(tdm[idx + (0:5), 101:110])


(freq_words <- findFreqTerms(tdm, lowfreq = 10))

term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq >=10)
df <- data.frame(term = names(term_freq), freq = term_freq)

####Plot of frequent terms#####

ggplot(df, aes(x=term, y=freq)) +
  geom_col()+
  xlab(NULL) +
             coord_flip() +
             theme_classic()+
             labs(x= "Count",
                  y = "Unique words",
                  title = "Unique word counts found in tweets")


###Ploting a wordcloud####

words_matrix <- as.matrix(tdm)

# calculate the frequency of words and sort it by frequency

word_freq <- sort(rowSums(words_matrix), decreasing = T)
wordcloud(words = names(word_freq), freq = word_freq, min.freq = 10,
          random.order = F)

