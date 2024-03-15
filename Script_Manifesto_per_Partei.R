install.packages("dplyr")
library(dplyr)
install.packages("NLP")
library(NLP)
install.packages("tm")
library(tm)
install.packages("manifestoR")
library(manifestoR)
mp_setapikey("C:/R/DigitalHumanities/manifesto_apikey.txt")
mpds <- mp_maindataset() 

#so wurden die Datensätze nach Partei gefiltert
#AFD

wanted <- data.frame(party=c(41953),
                     date=c(201309, 201709, 202109))
mp_corpus(wanted)

#CDU/CSU

wanted <- data.frame(party=c(41521),
                     date=c(199012, 199410, 199809, 200209, 200509, 200909, 201309, 201709, 202109))
mp_corpus(wanted)

#FDP

wanted <- data.frame(party=c(41420),
                     date=c(199012, 199410, 199809, 200209, 200509, 200909, 201309, 201709, 202109))
mp_corpus(wanted)

#SPD

wanted <- data.frame(party=c(41320),
                     date=c(199012, 199410, 199809, 200209, 200509, 200909, 201309, 201709, 202109))
mp_corpus(wanted)

#Die Linke 

wanted <- data.frame(party=c(41223),
                     date=c(200209, 200509, 200909, 201309, 201709, 202109))
mp_corpus(wanted)

#Die Grüne

wanted <- data.frame(party=c(41113),
                     date=c(199012, 199410, 199809, 200209, 200509, 200909, 201309, 201709, 202109))
mp_corpus(wanted)

#so wurden die Parteien-Datensätze nach Codes gefiltert

AFD <- mp_corpus(wanted, codefilter = c(501, 416.2))

corpus_cleaned <- tm_map(AFD, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus_cleaned <- tm_map(corpus_cleaned, removeNumbers)
corpus_cleaned <- tm_map(corpus_cleaned, stripWhitespace)
corpus_nostop <- tm_map(corpus_cleaned, removeWords, stopwords("german"))

#zuerst wurde festgestellt, dass die Codes richtig gefiltert wurden
head(codes(corpus_nostop))
print(corpus_nostop)

#danach sind sie als CSV-Dateien heruntergeladen
write.csv(corpus_nostop, "afd_limpio.csv")

