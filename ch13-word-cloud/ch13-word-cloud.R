# install.packages(c("tm", "SnowballC", "wordcloud"))

library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)

data <- read.csv('JEOPARDY_CSV.csv', stringsAsFactors = FALSE,
                 nrows = 10000)
dplyr::glimpse(data)

data_corpus <- Corpus(VectorSource(data$Question))
data_corpus
# ?Corpus
# data_corpus <- tm_map(data_corpus, PlainTextDocument)
# stopwords('english')

data_corpus <- tm_map(data_corpus, content_transformer(tolower))
as.character(data_corpus[[1]])
data_corpus <- tm_map(data_corpus, removePunctuation)
as.character(data_corpus[[1]])
data_corpus <- tm_map(data_corpus, removeWords, stopwords('english'))
as.character(data_corpus[[1]])

data_corpus <- tm_map(data_corpus, stemDocument)
as.character(data_corpus[[1]])

citation(package='wordcloud')
wordcloud(data_corpus, max.words=100, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))

data$Question[1]
as.character(data_corpus[[1]])

png("../plots/15-1.png", 5.5, 4, units='in', pointsize=9, res=600)
wordcloud(data_corpus, max.words=100, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))
dev.off()
