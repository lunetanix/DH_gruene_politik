install.packages("quanteda")
install.packages("topicmodels")
library(quanteda)
library(topicmodels)
require(quanteda)
require(topicmodels)

#Code nach dem Tutorial im Seminar

options(stringsAsFactors = FALSE)   

textdata <- read.csv("C:/R/DigitalHumanities/DH_Partei/proyecto_corpus.csv", sep = ",", encoding = "UTF-8")
corp_quanteda <- corpus(textdata$text, docnames = textdata$doc_id)

german_stopwords <- readLines("C:/R/DigitalHumanities/DH_Hausarbeit/stopwords_german.txt", encoding = "UTF-8")

corpus_tokens <- corp_quanteda %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = german_stopwords, padding = T) 
mp_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens,
                                                             min_count = 25) %>%
  mp_collocations_list <- as.list(mp_collocations)

mp_collocations <- mp_collocations[1:250, ]
corpus_tokens <- tokens_compound(corpus_tokens, mp_collocations_list)

DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

dim(DTM)

top10_terms <- c("deutschland", "bundesland", "bundesregierung", "tiere",
                 "regierung", "europa", "eu", "jahr", "grüne", "grüenen")

DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

require(topicmodels)

K <- 10
topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 900,
  seed = 1,
  verbose = 25,
  alpha = 0.02))

tmResult <- posterior(topicModel)
attributes(tmResult)

ncol(DTM) 

beta <- tmResult$terms 

dim(beta) 

rowSums(beta)

nrow(DTM)

theta <- tmResult$topics
dim(theta) 

rowSums(theta)[1:10] 

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

print(topicNames)

#LDA-Visualisierung

install.packages("LDAvis")
install.packages("tsne")

library(LDAvis)
library("tsne")

require(LDAvis)
require(tsne)
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)

topicToFilter <- grep("wirtschaft ", topicNames)[1]

topicThreshold <- 0.1
selectedDocumentIndexes <- (theta[, topicToFilter] >= topicThreshold)
filteredCorpus <- corp_quanteda %>%
  corpus_subset(subset = selectedDocumentIndexes)

filteredCorpus


library(reshape2)
library(ggplot2)
library(pals)

#Verteilung der Topics pro Partei

textdata$party_id <- paste0(substr(textdata$party_id, 0, 3), "0")

topic_proportion_per_party <- aggregate(theta,
                                        by = list(Partei = textdata$party_id), mean)
colnames(topic_proportion_per_party) [2:(K+1)] <- topicNames

vizDataFrame <- melt(topic_proportion_per_party, id.vars = "Partei")

ggplot(vizDataFrame,
       aes(x=Partei, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("Proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topics") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Verteilung der Topics pro Code (nicht in der Analyse einbezogen)

textdata$cmp_code <- paste0(substr(textdata$cmp_code, 0, 3), "0")

topic_proportion_per_code <- aggregate(theta,
                                        by = list(Code = textdata$cmp_code), mean)
colnames(topic_proportion_per_code) [2:(K+1)] <- topicNames

vizDataFrame <- melt(topic_proportion_per_code, id.vars = "Code")

ggplot(vizDataFrame,
       aes(x=Code, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("Proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topics") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

average_topic_proportions <- colMeans(beta)
dominant_topics <- max.col(DTM)

#Wordcloud nach einem YouTube-Video

install.packages("tm")
library(tm)
dtm <- TermDocumentMatrix(corpus_tokens)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)

install.packages("wordcloud")
install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)
wordcloud(words = d$word, freq = d$freq, min.freq = 4, max.words = 500, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(2, "Dark2"))

#Topic-Verteilung: 

average_topic_proportions <- colMeans(theta)
















