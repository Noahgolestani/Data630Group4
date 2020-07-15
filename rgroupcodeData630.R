install.packages(c("ggplot2", "e1071", "caret", "quanteda",
                   "irlba", "randomForest"))
install.packages("ggplot2")
install.packages("e1071")
install.packages("caret")
install.packages("quanteda", dependencies = TRUE)
install.packages("irlba")
install.packages("randomForest")
install.packages("fs")
install.packages("dplyr")
install.packages("tidytext")
install.packages("Dict")
install.packages("stringr", dependencies = TRUE)
#setwd("C:\\Users\\rajsi\\Documents\\UMGC Data Science\\DATA 630 9040 Machine Learning\\Group Project\\Analysis")

#Read in the tweets from Trump's Twitter Account:
tt.original <- read.csv("trumptweets.csv", head=T,stringsAsFactors = FALSE)
summary(tt.original)
str(tt.original)
#View(tt.original)

#Remove all other variables except the content (actual tweet text and date)
tt.raw = tt.original[,3:4]
names(tt.raw) = c("Tweet","Tdate")
#View(tt.raw)

#check for missing values
length(which(!complete.cases(tt.raw)))

#let's get a feel for the distribution of text lengths of the SMS
# messages by adding a new feature for the length of each message.
tt.raw$TextLength = nchar(tt.raw$Tweet)
summary(tt.raw$TextLength)

#Set seed and split data into training and test datasets.
set.seed(12345)

index = sample(2, nrow(tt.raw), replace = TRUE, prob = c(0.5, 0.5))

train = tt.raw[index == 1,]
test = tt.raw[index == 2,]

train$Tweet[33]

test$Tweet[33]


# There are many packages in the R ecosystem for performing text
# analytics. One of the newer packages in quanteda. The quanteda
# package has many useful functions for quickly and easily working
# with text data. Looks like better than tm

library(quanteda)
help(package = "quanteda")

# Tokenize tweeter messages.
train.tokens = tokens(train$Tweet,remove_numbers = T, remove_punct = T, remove_symbols = T,split_hyphens = T)

train.tokens[[33]]

# Lower case the tokens
train.tokens = tokens_tolower(train.tokens)
train.tokens[[33]]

# Use quanteda's built-in stopword list for English
stopwords()

train.tokens = tokens_select(train.tokens,stopwords(),selection = "remove")
train.tokens[[33]]

#Now that the words have been broken apart and stopwords removed lets get a readout of the different dictionaries to which we are messing with.
#Dictionary Counters:
library(dplyr)
library(tidytext)
library(Dict)
library(stringr)
#Read in the Lists for counting:
newscasters.original <- read.csv("newscasters.csv", head=T,stringsAsFactors = FALSE)
str(newscasters.original)
stocks.original <- read.csv("stocks.csv", head=T,stringsAsFactors = FALSE)
str(stocks.original)
#politicians.original <- read.csv("politicians.csv", head=T,stringsAsFactors = FALSE)
#str(politicians.original)

#https://cbail.github.io/SICSS_Dictionary-Based_Text_Analysis.html
#https://uc-r.github.io/pipe

#Newscaster Counter:
newscaster.dictionary<- str_c(newscasters.original$Name, collapse = ",")
newscaster.dictionary<-tolower(newscaster.dictionary)
newscaster.dictionary
newscaster.dictionary<-sapply(strsplit(newscaster.dictionary, '[,]+'), function(x) toString(dQuote(x)))
newscaster.dictionary
newscaster_tweets<-tidy_trump_tweets[str_detect(tidy_trump_tweets$content, newscaster.dictionary),]
head(newscaster_tweets$content)

#Debugging
test1.dict<- c("Tucker Carlson, “Sean Hannity”, “chris wallace”, “mark levin”, “neil cavuto”, “dana perino”, “bret baier”, “laura ingraham”, “jesse waters”, “jeanine pirro”, “martha maccallum”, “maria bartiromo”, “lou dobbs”, “charles payne”, “jim cramer”, “carl quintanilla”, “david faber”, “sara eisen”, “chuck todd”, “chris hayes”, “rachel maddow”, “brian williams”, “joe scarborough”, “wolf blitzer”, “jake tapper”, “brooke baldwin”, “erin burnett”, “anderson cooper”, “chris cuomo”, “don lemon”, “john berman”")
test1.tweets<-tt.original[str_detect(tt.original$content, test1.dict),]
head(test1.tweets$content)

test3.dict<- c("Tucker Carlson", "Sean Hannity")
test3.tweets<-tt.original[str_detect(tt.original$content, test3.dict),]
head(test3.tweets$content)



test2.dict<- newscaster.dictionary
test2.tweets<-tt.original[str_detect(tt.original$content, test2.dict),]
head(test2.tweets$content)


#S&P500 Counter:
#stocks.dictionary <- dict()
library(stringr)
snp.dictionary<- str_c(stocks.original$Name, collapse = ",")
snp.dictionary<-tolower(snp.dictionary)
snp.dictionary
snp_tweets<-tidy_trump_tweets[str_detect(tidy_trump_tweets$content, snp.dictionary),]
head(snp_tweets$content)

economic_dictionary<-c("economy","unemployment","trade","tariffs")
economic_tweets<-trumptweets[str_detect(trumptweets$text, economic_dictionary),]
head(economic_tweets$text)
#?stringr
#Primetime TV Counter:
#count.newscasters <- 0
#for (words in tidy_trump_tweets) {
#  if(newscasters.original$Name = tidy_trump_tweets)  count = count+1
#}
#print(count.newscasters)
#View(newscasters.original$Name)
#newscasters.dictionary <- dict(list(newscasters.original$Name))

#View(trumptweets)
tidy_trump_tweets<- tt.original %>%
  select(date,content) %>%
  unnest_tokens("word", content)
data("stop_words")

trump_tweet_top_words<-
  tidy_trump_tweets %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))
#select only top words
top_20<-trump_tweet_top_words[1:20,]

#create factor variable to sort by frequency
trump_tweet_top_words$word <- factor(trump_tweet_top_words$word, levels = trump_tweet_top_words$word[order(trump_tweet_top_words$n,decreasing=TRUE)])

#library(ggplot2)
#ggplot(top_20, aes(x=word, y=n, fill=word))+
#  geom_bar(stat="identity")+
#  theme_minimal()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  ylab("Number of Times Word Appears in Trump's Tweets")+
#  xlab("")+
#  guides(fill=FALSE)


#Politician Counter:




# Perform stemming on the tokens.
train.tokens =  tokens_wordstem(train.tokens,language = "english")

train.tokens[30:33]

# Create our first bag-of-words model. document frequency (feature) matrix
train.tokens.dfm <- dfm(train.tokens,tolower=F)

View(train)

# Transform to a matrix and inspect.
train.tokens.matrix = as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)
#View(train.tokens.matrix[1:20, 1:100])
dim(train.tokens.matrix)
# Investigate the effects of stemming.
colnames(train.tokens.matrix)[1:50]

# Setup a the feature data frame.
train.tokens.df <- convert(train.tokens.dfm , to= "data.frame")

# Cleanup column names.
names(train.tokens.df) <- make.names(names(train.tokens.df))

## IDK where Raj was going...


#Visualize Word Cloud Code:










