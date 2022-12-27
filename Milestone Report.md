---
title: "Milestone Report"
subtitle: "Capstone Project of the Data Science Specialization from Johns Hopkins University"
author: "Leonardo Simioni"
date: "2022-12-27"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

## Synopsis

This is the Milestone Report of the Capstone Project of the Data Science Specialiation from Johns Hopkins University. The main objective of this document is to explain the exploratory analysis made on the dataset and goals for the eventual app and algorithm. The motivation for this report is to download and load the data, create a report of summary statistics about the data and report interesting findings. 

The model will be trained using the English corpora on a unified document from the following text data which was provided by the JHU and SwitKey company: Blogs, News and Twitter.

## Loading the Data

```{r load-data, echo=TRUE}
trainURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
trainDataFile <- "data/Coursera-SwiftKey.zip"

if (!file.exists('data')) {
    dir.create('data')
}

if (!file.exists("data/final/en_US")) {
    tempFile <- tempfile()
    download.file(trainURL, tempFile)
    unzip(tempFile, exdir = "data")
    unlink(tempFile)
}

blogsFileName <- "data/final/en_US/en_US.blogs.txt"
con <- file(blogsFileName, open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

newsFileName <- "data/final/en_US/en_US.news.txt"
con <- file(newsFileName, open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

twitterFileName <- "data/final/en_US/en_US.twitter.txt"
con <- file(twitterFileName, open = "r")
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

rm(con)
```

## Summary Statistics

The data includes three separate text files which are going to be unified later for training. We are going to start by going through some basic statistics about this data set.

```{r}
library(stringi)
library(kableExtra)

sampleSize = 0.01

fileSizeMB <- round(file.info(c(blogsFileName,
                                newsFileName,
                                twitterFileName))$size / 1024 ^ 2)

numLines <- sapply(list(blogs, news, twitter), length)

numChars <- sapply(list(nchar(blogs), nchar(news), nchar(twitter)), sum)

numWords <- sapply(list(blogs, news, twitter), stri_stats_latex)[4,]

wpl <- lapply(list(blogs, news, twitter), function(x) stri_count_words(x))

wplSummary = sapply(list(blogs, news, twitter),
             function(x) summary(stri_count_words(x))[c('Min.', 'Mean', 'Max.')])
rownames(wplSummary) = c('WPL.Min', 'WPL.Mean', 'WPL.Max')

summary <- data.frame(
    File = c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt"),
    FileSize = paste(fileSizeMB, " MB"),
    Lines = numLines,
    Characters = numChars,
    Words = numWords,
    t(rbind(round(wplSummary)))
)

kable(summary,
      row.names = FALSE,
      align = c("l", rep("r", 7)),
      caption = "") %>% kable_styling(position = "left")
```

We can observe that Twitter has the most number of lines, followed by Blogs and News. Although the documents have a great number of lines, each corpora text has a somewhat low number of words per line, the reason Twitter has the lowest of them can be explained by its limitation of characters per tweet. 

It's important to keep in mind the size and runtime of the algorithm that will be developed on this project. To improve the amount of memory and time required to run it, a sample size of 1% of the data set above will be used.

## Data Preparation

As mentioned above, the data sets are going to be unified and sample to 1% to improve performance. Next, a Corpus will be built from this sampled data and cleaned by doing the following:
- Converting all word to lowercase and plain text documents
- Removing URL, stop words, whitespace, punctuation, numbers and profanity.

### Sampling and Cleaning the Data

```{r}
# setting seed
set.seed(690049)

# sampling all data sets
sampleBlogs <- sample(blogs, length(blogs) * sampleSize, replace = FALSE)
sampleNews <- sample(news, length(news) * sampleSize, replace = FALSE)
sampleTwitter <- sample(twitter, length(twitter) * sampleSize, replace = FALSE)

# removing non-English characters
sampleBlogs <- iconv(sampleBlogs, "latin1", "ASCII", sub = "")
sampleNews <- iconv(sampleNews, "latin1", "ASCII", sub = "")
sampleTwitter <- iconv(sampleTwitter, "latin1", "ASCII", sub = "")

# combining all data sets into a single file
sampleData <- c(sampleBlogs, sampleNews, sampleTwitter)
sampleDataFileName <- "data/final/en_US/en_US.sample.txt"
con <- file(sampleDataFileName, open = "w")
writeLines(sampleData, con)
close(con)

sampleDataLines <- length(sampleData);
sampleDataWords <- sum(stri_count_words(sampleData))
rm(blogs, news, twitter, sampleBlogs, sampleNews, sampleTwitter)
```

### Building Corpus

```{r}
library(tm)

# downloading bad words file for filtering
badWordsURL <- "https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/badwordslist/badwords.txt"
badwords <- "data/badwords.txt"
if (!file.exists('data')) {
    dir.create('data')
}
if (!file.exists(badwords)) {
    tempFile <- tempfile()
    download.file(badWordsURL, tempFile)
    unzip(tempFile, exdir = "data")
    unlink(tempFile)
}

buildCorpus <- function (dataSet) {
    docs <- VCorpus(VectorSource(dataSet))
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    
# removing URL, Twitter handles and email patterns
    docs <- tm_map(docs, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
    docs <- tm_map(docs, toSpace, "@[^\\s]+")
    docs <- tm_map(docs, toSpace, "\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b")
    
# removing bad  words
    con <- file(badwords, open = "r")
    profanity <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
    close(con)
    profanity <- iconv(profanity, "latin1", "ASCII", sub = "")
    docs <- tm_map(docs, removeWords, profanity)
    
    docs <- tm_map(docs, tolower)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs, PlainTextDocument)
    return(docs)
}

# building corpus
corpus <- buildCorpus(sampleData)
saveRDS(corpus, file = "data/final/en_US/en_US.corpus.rds")

# converting corpus to a dataframe
corpusText <- data.frame(text = unlist(sapply(corpus, '[', "content")), stringsAsFactors = FALSE)
con <- file("data/final/en_US/en_US.corpus.txt", open = "w")
writeLines(corpusText$text, con)
close(con)

rm(sampleData)
```

## Data Exploring

Data exploration is the main purpose of this report and it will be comprised of analyzing the most frequent words, tokens and n-grams in the dataset.

### Plotting a bar chart of the 10 most frequent words

```{r}
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

tdm <- TermDocumentMatrix(corpus)
freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
wordFreq <- data.frame(word = names(freq), freq = freq)

g <- ggplot (wordFreq[1:10,], aes(x = reorder(wordFreq[1:10,]$word, -wordFreq[1:10,]$fre),
                                  y = wordFreq[1:10,]$fre ))
g <- g + geom_bar( stat = "Identity" , fill = I("blue"))
g <- g + geom_text(aes(label = wordFreq[1:10,]$fre), vjust = -0.20, size = 3)
g <- g + xlab("")
g <- g + ylab("Word Frequency")
g <- g + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
               axis.text.x = element_text(hjust = 0.5, vjust = 0.5, angle = 45),
               axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
g <- g + ggtitle("Top 10 Words")
print(g)
```

### Plotting a word cloud of the 100 most frequent words

```{r}
suppressWarnings (
    wordcloud(words = wordFreq$word,
              freq = wordFreq$freq,
              min.freq = 1,
              max.words = 100,
              random.order = FALSE,
              rot.per = 0.35, 
              colors=brewer.pal(8, "Dark2"))
)

rm(tdm, freq, wordFreq, g)
```

### Tokenizing and n-gram frequency

```{r}

library(RWeka)

unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
```

### Plotting the 15 most frequent Unigrams

```{r}
unigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = unigramTokenizer))

unigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(unigramMatrix, 0.99))), decreasing = TRUE)
unigramMatrixFreq <- data.frame(word = names(unigramMatrixFreq), freq = unigramMatrixFreq)

g <- ggplot(unigramMatrixFreq[1:15,], aes(x = reorder(word, -freq), y = freq))
g <- g + geom_bar(stat = "identity", fill = I("red"))
g <- g + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
g <- g + xlab("")
g <- g + ylab("Frequency")
g <- g + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
               axis.text.x = element_text(hjust = 1.0, angle = 45),
               axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
g <- g + ggtitle("Top 15 Unigrams")
print(g)
```

### Plotting the 15 most frequent Bigrams

```{r}
bigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = bigramTokenizer))

bigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(bigramMatrix, 0.999))), decreasing = TRUE)
bigramMatrixFreq <- data.frame(word = names(bigramMatrixFreq), freq = bigramMatrixFreq)

g <- ggplot(bigramMatrixFreq[1:15,], aes(x = reorder(word, -freq), y = freq))
g <- g + geom_bar(stat = "identity", fill = I("red"))
g <- g + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
g <- g + xlab("")
g <- g + ylab("Frequency")
g <- g + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
               axis.text.x = element_text(hjust = 1.0, angle = 45),
               axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
g <- g + ggtitle("Top 15 Bigrams")
print(g)
```

### Plotting the 15 most frequent Trigrams

```{r}
trigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = trigramTokenizer))

trigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(trigramMatrix, 0.9999))), decreasing = TRUE)
trigramMatrixFreq <- data.frame(word = names(trigramMatrixFreq), freq = trigramMatrixFreq)

g <- ggplot(trigramMatrixFreq[1:15,], aes(x = reorder(word, -freq), y = freq))
g <- g + geom_bar(stat = "identity", fill = I("red"))
g <- g + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
g <- g + xlab("")
g <- g + ylab("Frequency")
g <- g + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
               axis.text.x = element_text(hjust = 1.0, angle = 45),
               axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
g <- g + ggtitle("Top 15 Trigrams")
print(g)
```

## Moving Forward

The knowledge obtained from the analysis made so far will be helpful for the next steps of this Capstone Project, which comprises of:
- Building and evaluating a prediction model
- Reducing computational runtime and model complexity
- Developing a data product
- Slide deck presentation of the data product



