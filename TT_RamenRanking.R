library(tidyverse)
library(DataExplorer)

##do the ramen thing
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

##check out the data
introduce(ramen_ratings)
profile_missing(ramen_ratings)
table(ramen_ratings$country)

##make corrections to the data
ramen_ratings <- ramen_ratings %>%
  mutate(
  country = case_when(country == "Phlippines" ~ "Philippines", country == "USA" ~ "United States", TRUE ~ country))

##make a basic map of ratings
library(rworldmap)

#join to a coarse resolution map
rrmap <- joinCountryData2Map(ramen_ratings, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(rrmap, nameColumnToPlot = "stars", catMethod = "fixedWidth", mapTitle = NA, colourPalette = "heat")
?mapCountryData
##get just the main styles of noodles
table(ramen_ratings$style)

ramen_ratings_sub <- ramen_ratings %>%
  filter(style == c("Bowl", "Cup", "Pack", "Tray"))
?filter

library(ggplot2)

ramenplot <- ggplot(ramen_ratings_sub, aes(x = country, y = stars)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ramen_ratings_sub$variety


# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

docs <- Corpus(VectorSource(ramen_ratings_sub$variety))
inspect(docs)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove stop words
docs <- tm_map(docs, removeWords, c("noodles", "noodle", "ramen", "flavor", "flavour", "soup", "cup", "instant")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wc <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set2"))
