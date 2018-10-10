library(easyPubMed)
library(pubmed.mineR)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)


morin_query = "Ryan Morin[AUTH]"

morin_on_pubmed <- get_pubmed_ids(morin_query)
morin_abstracts_xml <- fetch_pubmed_data(morin_on_pubmed)

my_text <- unlist(xpathApply(morin_abstracts_xml, "//AbstractText", saveXML))

cleanFun <- function(xmlString) {
  return(gsub("<.*?>", "", xmlString))
}

clean_text = lapply(my_text,cleanFun) #as a list. Need to flatten

clean_text_v = unlist(clean_text)

vs <- VectorSource(clean_text_v)
docs = VCorpus(vs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify stopwords as a character vector
docs <- tm_map(docs, removeWords, c("approach","observed","overall","identify","using","bcl","normal","however","known","found", "within","samples","show","revealed","used","can","new","single","study","mir","including")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(12, "Paired"))

