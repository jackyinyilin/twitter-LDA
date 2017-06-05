LDAvis/R/createJSON.R
###Version 1.0.0
#Author: Yilin Yin
#Date June 2nd 2017
#
#packages.
library(twitteR)
library(MASS)
library(topicmodels)
library(tidytext)
library(stringr)
library(dplyr)
library(tm)
library(slam)
library(ggplot2)
library(LDAvis)
library(proxy)
library(stringi)
library(servr)

#call for API authorization from Twitter. 
consumer_key <- "#Your consumer key#"
consumer_secret <- "#Your consumer secret#"
access_token <- "#Your access token#"
access_secret <- "#Your access secret#"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#get the tweets.
#set up the real-time key words and get the string from the twitter.
tweets <- searchTwitter("#Key Words#", n=10000, lang="en", since="2004-08-20")
#add each string into data frame.
data <- do.call("rbind", lapply(tweets, as.data.frame))
#make a copy.
df <- data


#remove duplication.
df$text <- gsub("RT", " ", df$text)  # Remove the "RT" (retweet) so duplicates are duplicates
df$text <- gsub("@\\w+", " ", df$text)  # Remove user names 
df$text <- gsub("http.+ |http.+$", " ", df$text)  # Remove links
df$text <- gsub("[[:punct:]]", " ", df$text)  # Remove punctuation
df$text <- gsub("[ |\t]{2,}", " ", df$text)  # Remove tabs
df$text <- gsub("amp", " ", df$text)  # "&" is "&amp" in HTML, so after punctuation removed ...
df$text <- gsub("^ ", "", df$text)  # Leading blanks
df$text <- gsub(" $", "", df$text)  # Lagging blanks
df$text <- gsub(" +", " ", df$text) # General spaces 
df$text <- str_replace_all(df$text, "[^[:alnum:]]", " ")#remove non-characters


library(qdap)
stopw <- stopwords("english")

#length(stopw)
#stopw <- str_pad(stopw, 5, "both")

stopw <- paste("",stopw,"")

stopw
stopw <- as.character(stopw)

stopw[3]

for (i in 1:length(stopw)){
 df$text <- gsub(stopw[i], " ", df$text) # General stopword
}



df$text <- tolower(df$text)  # Make everything consistently lower case
clean.df <- df[!duplicated(df$text),]  # Now get rid of duplicates



clean.df$text <- as.character(clean.df$text)

d <- data.frame(clean.df$text)

#tokenizise the text and make it into docu term matrix
mycorpus <- Corpus(DataframeSource(d))
#mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))#remove stop word

tdm <- DocumentTermMatrix(mycorpus)

#get the tf idf to remove the low freq and zero freq row.
term_tfidf <- tapply(tdm$v/row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm)/col_sums(tdm > 0))
#summary(term_tfidf)
tdm <- tdm[,term_tfidf >= 0.1]
tdm <- tdm[row_sums(tdm) > 0,]
#summary(col_sums(tdm))

#find the best k by maximum log likelihood.
best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(tdm, d)})

best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))

which.max(as.numeric(best.model.logLik$V1))
#get 48 

#create LDA model.
ap_lda <- LDA(tdm, k = 10, control = list(seed = 2440))

ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_topics

#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param corpus Corpus object used to create the document term
#' matrix for the \code{LDA} model. This should have been create with
#' the tm package's \code{Corpus} function.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's 
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export


topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
 # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    doc_length <- doc_length[doc_length != 0] #remove )
  }
  temp_frequency <- as.matrix(tdm)# changed from original inspect into as.matrix to get the whole table.
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

jsonf <- topicmodels_json_ldavis(ap_lda,mycorpus,tdm)

serVis(jsonf, out.dir = './vis', 
       open.browser = FALSE)

#network
#term network
#get the term lower than 100 times off.
freq.terms <- findFreqTerms(tdm, lowfreq = 100)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)

library(ggnetwork)
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(Rgraphviz)

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

#user network(not finished yet)
k <- clean.df$text %>%
  str_split(", | and ") %>%
  unlist %>%
  unique %>%
  sort

m <- sapply(clean.df$text, function(x) {
  str_split(x, ", | and ") %>% unlist
}) %>%
  lapply(function(x) { as.integer(k %in% x) }) %>%
  unlist %>%
  matrix(ncol = length(k), byrow = TRUE)

rownames(m) <- clean.df$screenName
colnames(m) <- k 

n <- ggnetwork(m)
n$user <- n$vertex.names %in% clean.df$screenName

ggplot(n, aes(x, y, xend = xend, yend = yend)) + 
  geom_edges(color = "grey50") + 
  geom_nodelabel(data = n[ !n$user, ],
                 aes(label = vertex.names),
                 color = "grey50", label.size = NA) +
  geom_nodelabel(data = n[ n$user, ],
                 aes(label = vertex.names),
                 color = "steelblue", fontface = "bold") +
  theme_blank()
