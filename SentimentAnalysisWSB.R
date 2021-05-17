#Jonas Schmitten & Noah Angara & Desiree Tran
#Big Data Analytics
#May 2021

#packages
library(data.table)
library(dplyr)
library(anytime)
library(tibble)
library(tidyverse)
library(vader)
library(chron)
library(quantmod)
library(tidyquant)
library(pryr)
library(compare)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#setting working directory
Paths = c("/Users/jonasschmitten/Downloads/Sentiment_Analysis_WSB", 
          "/Users/noahangara/Downloads")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

#download "source package" of vader under https://cran.r-project.org/web/packages/vader/index.html
#make sure you put the source file into the same working directory

load("vader/R/sysdata.rda")

#partially reading in data 
data = as.data.frame(fread('wsb_comments_raw.csv', nrows = 10000))
#alternative? to check if fread skips rows
#data = read.csv("wsb_comments_raw.csv",nrows=10000)
#remove all columns where only NAs
data = data[,colSums(is.na(data))<nrow(data)]

#remove columns with unimportant information. Check with unique() first 
data = select(data, -c(author_flair_background_color, author_flair_css_class,author_flair_template_id,author_flair_text_color,
                       author_flair_type, author_patreon_flair, awarders, gildings, locked, retrieved_on, send_replies, stickied, subreddit, 
                       subreddit_id, treatment_tags, edited, author_cakeday))

#author_flair_text and author_flair_richtext basically the same?
#what is no_follow?
#what is score? upvotes?
#not sure about distinguished

#convert UNIX to UTC

data = data[order(data$created_utc),]

#reset index after odering 
row.names(data) <- NULL

data$created_utc = anytime::utctime(data$created_utc)


#countLines('wsb_comments_raw.csv')

#create date and time columns 
data = add_column(data, Date = substr(data$created_utc, 1, 10),.before = 1)
data = add_column(data, Time = substr(data$created_utc, 12, 19),.after = 1)

#drop no longer needed column
#data = select(data,-created_utc)

#change date and time formats
data$Date = as.Date(data$Date)
data$Time = times(data$Time)

data = data[order(data$Date, data$Time),]

#reset index after ordering 
row.names(data) <- NULL
# Adding Words to Vader Dictionary ----------------------------------------

# let's add some words to the dictionary that are specific to WSB
wsbLexicon <- bind_rows(tibble(V1 = c("retard", "retarded", "fuck", "fucking", "autist", "fag", "faggot", "gay", "stonk", "porn", 
                                      "degenerate", "boomer", "ape", "gorilla", "shit"), V2 = 0, V3 = 0.5), # neutral 
                        tibble(V1 = c("bull", "bullish", "tendie", "tendies", "call", "long", "buy", "moon", "hold",# positive
                                      "diamond", "hands", "yolo", "yoloed", "free", "btfd", "rocket", "elon", "gain",
                                      "420", "calls", "longs", "sky", "space", "roof", "squeeze", "balls", "JPOW", "printer",
                                      "brrr", "HODL", "daddy", "BTFD", "squoze", "full moon", "full moon face", "ox", "astronaut",
                                      "man astronaut", "gem stone", "money bag", "green", "profit"), V2 = 3, V3 = 0.5),                     
                        tibble(V1 = c("bear", "sell", "put", "short", "shorts", "puts", "bagholder", "wife", "boyfriend",# negative
                                      "shorting", "citron", "hedge", "fake", "virgin", "cuck", "guh", "paper", "SEC", "drilling",
                                      "bear face", "briefcase", "roll of paper", "red"), V2 = -1.5, V3 = 0.5))

# add back to lexicon
vaderLexiconWSB <- vaderLexicon %>% 
  as_tibble() %>% 
  # anti_join(wsbLexicon, by = "V1") %>% 
  filter(!(V1 %in% wsbLexicon$V1)) %>% 
  bind_rows(wsbLexicon) %>% 
  as.data.frame()
# change lexicon to include new words
vaderLexicon <- vaderLexiconWSB

save(vaderLexicon, file ="vader/R/sysdata.rda")

# remove the vader package and reinstall
detach("package:vader", unload = T)
remove.packages("vader")
#install package which contains the new words
install.packages("vader/", repos = NULL, type = "source")
#load the new package
library(vader)

#TEXT NORMALISATION -----------------------------------------------------------------------------------------
#data$body = toupper(data$body)


#Get all stock tickers traded in the US
stock_tickers = read.csv("stock_tickers.csv")

#CHECK WHICH STOCKS HAVE TICKERS SAME AS LETTERS AND IF RELEVANT TO KEEP
reg_expression <- regex(paste0("\\b(?:",
                               paste(stock_tickers$Symbol, collapse = "|"),
                               ")\\b"))
#paste(paste(stock_tickers$Symbol, collapse = "|"), paste(word(stock_tickers$Name), collapse = "|"), collapes = "|")

reddit_mentions <- data %>%
  mutate(stock_mention = str_extract_all(body, reg_expression)) %>%
  unnest(cols = stock_mention)

rm(data)
gc()

#read stopwords text
stop_words = read_tsv("stop_words_english.txt")

#Create Word Cloud of Comments
word_cloud <- Corpus(VectorSource(reddit_mentions$body))
# Convert the text to lower case
word_cloud <- tm_map(word_cloud, content_transformer(tolower))
# Remove numbers
word_cloud <- tm_map(word_cloud, removeNumbers)
# Remove english common stopwords
word_cloud <- tm_map(word_cloud, removeWords, stopwords("english"))
word_cloud <- tm_map(word_cloud, removeWords, t(stop_words))
word_cloud <- tm_map(word_cloud, removeWords, c("’", "finally", "making", "couple", "people", "feel", "time"))
# Remove punctuations
word_cloud <- tm_map(word_cloud, removePunctuation)
# Eliminate extra white spaces
word_cloud <- tm_map(word_cloud, stripWhitespace)

#Add Elements to matrix and count number of words
dtm <- TermDocumentMatrix(word_cloud)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(2,0.4))


reddit_mention_counts <- reddit_mentions %>% 
  group_by(Date, stock_mention) %>% 
  count()
reddit_mention_counts$Month = format(as.Date(reddit_mention_counts$Date), "%Y-%m")


# false positive acronyms which could be mistaken as tickers (non-stock related):
for (i in LETTERS){
  print(stock_tickers%>%filter(stock_tickers$Symbol == i))
  }

fp <- c("RH", "DD", "CEO", "IMO", "EV", "PM", "TD", "ALL", "USA", "IT", "EOD", "ATH",
        "IQ", LETTERS)

#return the 5 most mentioned stocks by month
monthly_top5 = reddit_mention_counts %>%
  group_by(Month, stock_mention) %>%
  summarise(n = sum(n)) %>%
  filter(!(stock_mention %in% fp)) %>% 
  top_n(5)


#get top 5 stocks mentioned in the data
top5 <- reddit_mention_counts %>% 
  group_by(stock_mention) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n) %>%
  filter(!(stock_mention %in% fp)) %>% 
  head(5) %>% 
  pull(stock_mention)

#plot mentions of top5 stocks 
# reddit_mention_counts %>% 
#   filter(stock_mention %in% top5) %>% 
#   ggplot(aes(x = Date, y = n, color = stock_mention)) + geom_line() #+ theme_classic()

#apply vader to get sentiment of comments from stocks (but only most mentioned stocks)
comments_sentiment = reddit_mentions %>%
  filter(stock_mention %in% unique(monthly_top5$stock_mention))%>%
  select(body) %>%
  distinct() %>%
  mutate(comment_clean = str_replace_all(body, "\\\\", " ")) %>%
  mutate(sentiment = vader_df(comment_clean)$compound)

#add sentiment to mentions df but only for top 5 monthly stocks
reddit_mentions = reddit_mentions %>%
  filter(stock_mention %in% unique(monthly_top5$stock_mention))

reddit_mentions_sentiment <- reddit_mentions %>% 
  left_join(comments_sentiment %>% select(-comment_clean),
            by = "body")

#sentiment by day and stock
reddit_sentiment_counts <- reddit_mentions_sentiment %>% 
  group_by(Date, stock_mention) %>% 
  summarise(sentiment = mean(sentiment, na.rm = T),
            n = n())

#create portfolio of stocks
reddit_sentiment_counts$Month = format(as.Date(reddit_sentiment_counts$Date), "%Y-%m")
sentiment_portfolio <- reddit_sentiment_counts %>%
  group_by(Month, stock_mention) %>%
  summarise(n = sum(n), sentiment = mean(sentiment, na.rm = T)) %>%
  arrange(Month, -n) %>%
  filter(sentiment > 0) %>%
  slice_head(n = 5) %>%
  mutate(id = row_number())

#arrange df by stocks
portfolio_stocks = sentiment_portfolio %>%
  group_by(Month, stock_mention) %>%
  pivot_wider(id_cols = Month, names_from = id, values_from = stock_mention, names_sep = "")
portfolio_stocks$Month = as.Date(paste(portfolio_stocks$Month,"-01",sep=""))+31
portfolio_stocks = as.data.frame(portfolio_stocks)


#get value of five stocks each month (i.e. row) 
portfolio_value = vector("list", nrow(portfolio_stocks)) #pre-allocate memory
for (row in 1:nrow(portfolio_stocks)) {
  stuff = tq_get(paste(portfolio_stocks[row, c(2:6)]),get = "stock.prices", from = portfolio_stocks[row, 1], to = portfolio_stocks[row, 1]+31) %>%
    group_by(date)%>%
    summarise(PortfolioValue = sum(close))%>%
    filter(row_number() %in% c(1, n()))
  portfolio_value[[row]] = stuff
}

#which stocks enter portfolio new, which ones do we hold
stocks_held_list = vector("list", length = nrow(sentiment_portfolio))
stocks_added_list = vector("list", length = nrow(sentiment_portfolio))
for (row in 1:nrow(sentiment_portfolio)){
  stocks_held = as.data.frame(sentiment_portfolio[row, which(sentiment_portfolio[row,] %in% sentiment_portfolio[row-1,])])
  stocks_added = as.data.frame(sentiment_portfolio[row, which(!(sentiment_portfolio[row,] %in% sentiment_portfolio[row-1,]))])
  stocks_held_list[[row]] = stocks_held
  stocks_added_list[[row]] = stocks_added
}

stocks_held_df = bind_rows(stocks_held_list)
stocks_added_df = bind_rows(stocks_added_list)

#value of stocks as DataFrame
value_of_stocks <- data.frame(matrix(unlist(portfolio_value), nrow=length(portfolio_value), byrow=TRUE))
value_of_stocks$X1 = as.Date(value_of_stocks$X1)
value_of_stocks$X2 = as.Date(value_of_stocks$X2)
colnames(value_of_stocks) = c("Hold Begin", "Hold End", "Begin Price", "End Price")

#calculate performance


#plot sentiment of portfolio with value
sentiment_portfolio %>%
  group_by(Month) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = T)) %>%
  ggplot() + geom_line(aes(x = Month, y = mean_sentiment, group = 1))


#get value of S&P500 as benchmark
getSymbols("SPY", src = "yahoo", from = value_of_stocks[1,1], to = value_of_stocks[nrow(value_of_stocks),1])


#plot sentiment over time
reddit_sentiment_counts %>% 
  filter(stock_mention %in% top5) %>% ggplot(aes(x = Date, y = sentiment, color = stock_mention)) +geom_smooth(se = F)


# #Getting stock prices based on most mentioned stocks
# getSymbols(sentiment_portfolio, src = "yahoo", from = '2020-02-02', to = '2021-05-10')
# 
# stock_prices = map(top5,function(x) Ad(get(x)))
# stock_prices = reduce(stock_prices, merge)
# colnames(stock_prices) = top5
# 
# #Remove unnecessary variables 
# for (i in 1:length(top5)){
#   rm(list = top5[i])
# }





