install.packages(c("ggplot2", "e1071", "caret", "quanteda",
                   "irlba", "randomForest"))
install.packages("ggplot2")
install.packages("e1071")
install.packages("caret")
install.packages("quanteda", dependencies = TRUE)
install.packages("irlba")
install.packages("randomForest")
install.packages("fs")

#setwd("C:\\Users\\rajsi\\Documents\\UMGC Data Science\\DATA 630 9040 Machine Learning\\Group Project\\Analysis")
tt.original <- read.csv("trumptweets.csv", head=T,stringsAsFactors = FALSE)
summary(tt.original)
str(tt.original)
View(tt.original)

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




