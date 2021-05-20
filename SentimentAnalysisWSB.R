#Jonas Schmitten and Noah Angara
#Big Data Analytics
#May 2021

#packages used (locally and on AWS)
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
library(zoo)
library(lubridate)
library(scales)
library(aws.s3)
library(readr)


# AWS Setup ---------------------------------------------------------------

#Default Home Directory of Rstudio on this AMI is limited to 10GB (we think) so need to change wd
setwd("/dev")

#AWS Access key to access S3 bucket
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "",
           "AWS_DEFAULT_REGION" = "")

#Read in Data from Amazon S3 Bucket
stock_tickers = aws.s3::s3read_using(read.csv, object = "s3://noahangara/stock_tickers.csv")
stock_tickers = stock_tickers[stock_tickers$Market.Cap>1000000000,]

data = aws.s3::s3read_using(read_csv, object = "s3://noahangara/wsb_comments_raw.csv")



# Data Cleaning -----------------------------------------------------------

#Select only Date Column and Body which contains the comments/posts
data = data %>% select(body, created_utc)
gc()

#Remove NA's
data = data %>% drop_na()
gc()

#order data by UNIX time
data = data[order(data$created_utc),]

#reset index after odering 
row.names(data) <- NULL

#convert UNIX to UTC time
data$created_utc = anytime::utctime(data$created_utc)

# remove 1969/1970 values as these contain no characters in body
data = data[which(!grepl("1969", data$created_utc)), ]
data = data[which(!grepl("1970", data$created_utc)), ]
gc()

#create date and time columns 
data = add_column(data, Date = substr(data$created_utc, 1, 10),.before = 1)
data = add_column(data, Time = substr(data$created_utc, 12, 19),.after = 1)
gc()

#drop no longer needed column
data = data %>% select(-created_utc)
gc()

#change date and time formats
data$Date = as.Date(data$Date)
data$Time = times(data$Time)

#order data by Date and Time
data = data[order(data$Date, data$Time),]

#reset index after ordering 
row.names(data) <- NULL

#remove [deleted] and [removed]
data = data[which(data$body != "[removed]"), ]
data = data[which(data$body != "[deleted]"), ]
gc()

#partition data into 2018, 2019, 2020, 2021
data2018 = data[which(grepl("2018", data$Date)),]

# Adjustment to Sentiment Dictionary ----------------------------------------

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

#manipulate package by saving sysdata file
save(vaderLexicon, file ="vader/R/sysdata.rda")

# remove the vader package and reinstall (might have to restart R session)
detach("package:vader", unload = T)
remove.packages("vader")
#install package which contains the new words
install.packages("vader/", repos = NULL, type = "source")
#load the new package
library(vader)

#Sentiment/Mentions of Tickers -----------------------------------------------------------------------------------------
#data$body = toupper(data$body)


#Get all stock tickers traded in the US
stock_tickers = read.csv("stock_tickers.csv")
stock_tickers = stock_tickers[stock_tickers$Market.Cap>300000000,]
stock_tickers <- stock_tickers[!(stock_tickers$Symbol %in% c("RH", "DD", "CEO", "IMO", "EV", "PM", "TD", "ALL", "USA", "IT", "EOD", "ATH",
                                                            "IQ", "TDA", "IDE", "BE", "AM", "DSP", "FREE", "CC", "AMP", "VTIQ", "NOM", LETTERS)),]

#CHECK WHICH STOCKS HAVE TICKERS SAME AS LETTERS AND IF RELEVANT TO KEEP
reg_expression <- regex(paste0("\\b(?:",
                               paste(stock_tickers$Symbol, collapse = "|"),
                               ")\\b"))
#paste(paste(stock_tickers$Symbol, collapse = "|"), paste(word(stock_tickers$Name), collapse = "|"), collapes = "|")


#get comments with stock tickers
reddit_mentions <- data.table(data) %>%
  mutate(stock_mention = str_extract_all(body, reg_expression)) %>%
  unnest(cols = stock_mention)


#remove data as we have all information in other df
rm(data)
gc()

#count number of stock mentions by day
reddit_mention_counts <- reddit_mentions %>% 
  group_by(Date, stock_mention) %>% 
  count()
reddit_mention_counts$Month = format(as.Date(reddit_mention_counts$Date), "%Y-%m")


# false positive acronyms which could be mistaken as tickers (non-stock related):
for (i in LETTERS){
  print(stock_tickers%>%filter(stock_tickers$Symbol == i))
  }

#False Positive Stocks (have tickers but most likely not what is meant)
fp <- c("RH", "DD", "CEO", "IMO", "EV", "PM", "TD", "ALL", "USA", "IT", "EOD", "ATH",
        "IQ", "TDA", "IDE", "BE", "AM", "DSP", "FREE", "CC", "AMP", "VTIQ", "NOM", LETTERS)

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

#add sentiment to mention of ticker
reddit_mentions_sentiment <- reddit_mentions %>% 
  left_join(comments_sentiment %>% select(-comment_clean),
            by = "body")

#sentiment by day and stock
reddit_sentiment_counts <- reddit_mentions_sentiment %>% 
  group_by(Date, stock_mention) %>% 
  summarise(sentiment = mean(sentiment, na.rm = T),
            n = n())
rm(comments_sentiment, reddit_mentions, reddit_mentions_sentiment)
gc()


# Create Portfolio from Comments ------------------------------------------
#create portfolio of stocks
reddit_mention_counts$Week = paste0(substr(reddit_mention_counts$Date, 1, 4), "-", strftime(reddit_mention_counts$Date, format = "%V"))
sentiment_portfolio <- reddit_mention_counts %>%
  group_by(Week, stock_mention) %>%
  summarise(n = sum(n)) %>%
  arrange(Week, -n) %>%
  # filter(sentiment > 0) %>%
  slice_head(n = 5) %>%
  mutate(id = row_number())

#arrange df by stocks
portfolio_stocks = sentiment_portfolio %>%
  group_by(Week, stock_mention) %>%
  pivot_wider(id_cols = Week, names_from = id, values_from = stock_mention, names_sep = "")
portfolio_stocks = na.locf(portfolio_stocks)
portfolio_stocks = as.data.frame(portfolio_stocks)

#add back to monthly format to get stock prices (tq_get need standard format)
portfolio_stocks$Week = as.Date(paste(portfolio_stocks$Week, 1, sep="-"), "%Y-%U-%u")
portfolio_stocks = read_csv("portfolio_stocks.csv")
portfolio_stocks$Week[152] = as.Date("2021-01-02") #add missing date
portfolio_stocks$Week[160] = as.Date("2021-02-22") #add missing date
portfolio_stocks = data.frame(portfolio_stocks)

#get value of five stocks each period (i.e. row) 
stock_price_list = vector("list", length = nrow(portfolio_stocks))
for (row in 1:nrow(portfolio_stocks)) {
  stock_prices = data.frame(tq_get(paste(portfolio_stocks[row, c(2:11)]), get = "stock.prices", 
                                     from = portfolio_stocks[row, 1], to = ceiling_date(portfolio_stocks$Week[row], "week")-days(1))) %>%
    # get opening and closing prices
    group_by(symbol)%>%
    # get beginning and end of period as we buy at beginning and sell at end
    filter(row_number() %in% c(1, n()))
  stock_price_list[[row]] = stock_prices
}
#df with opening and closing prices
stock_df = bind_rows(stock_price_list)
stock_df$value = NA

#arrange df by date and ticker
stock_df = stock_df %>% 
  arrange(date, symbol)

#some dates do not contain 10 stocks, problematic for loop below
stock_df = stock_df[which(!(grepl("2019-03-18", stock_df$date))),]
stock_df = stock_df[which(!(grepl("2019-03-22", stock_df$date))),]
stock_df = stock_df %>%
  select(-stock.prices)

#starting capital of investment strategy
pf_value <- 100000
diff_pf <- 0

#Loop to Calculate Portfolio Value iteratively
k = 0
for (i in 1:nrow(stock_df)){
  k = k + 1 
  stock_df$value[i] <- floor((1/(ncol(portfolio_stocks)-1) * pf_value)/stock_df$open[i]) * stock_df$open[i]
  if (k == 10) {
    diff_pf <- pf_value - sum(stock_df$value[(i-9):i])
  }
  if (k > 10 ) {
    stock_df$value[i] <- floor((1/(ncol(portfolio_stocks)-1) * pf_value)/stock_df$open[i-10]) * stock_df$close[i]
    if (k == 20) {
      pf_value <- sum(stock_df$value[(i-9):i]) + diff_pf
      k = 0 
    }
  }
}

#calculate value of portfolio on a monthly basis
monthly_sum = stock_df %>%
  group_by(date) %>%
  summarise(reddit_portfolio = sum(value))

#Pull SPY as a benchmark against reddit portfolio
getSymbols("SPY", from = "2018-01-08", to = "2021-02-26")
SPY = SPY[monthly_sum$date, ]
monthly_sum = head(monthly_sum, 315)
monthly_sum$SPY = SPY*365.8848 #scale to value of reddit portfolio (100'000)
monthly_sum = melt(monthly_sum, "date")



# Visualization of Results ------------------------------------------------

#plot monthly value of reddit portfolio vs benchmark
ggplot(monthly_sum) + geom_line(aes(x = date, y = value, color = variable)) + 
  scale_y_continuous(labels = comma)

#read stopwords text
stop_words = read_tsv("stop_words_english.txt")

#Create Word Cloud of Comments
word_cloud <- Corpus(VectorSource(reddit_mentions$body))
# Convert the text to lower case
word_cloud <- tm_map(word_cloud, content_transformer(tolower))
# Remove numbers
word_cloud <- tm_map(word_cloud, removeNumbers)
# Remove english common stopwords as well as custom ones
word_cloud <- tm_map(word_cloud, removeWords, stopwords("english"))
word_cloud <- tm_map(word_cloud, removeWords, t(stop_words))
word_cloud <- tm_map(word_cloud, removeWords, c("’", "finally", "making", "couple", "people", "feel", "time"))
# Remove punctuation
word_cloud <- tm_map(word_cloud, removePunctuation)
# Eliminate extra white spaces
word_cloud <- tm_map(word_cloud, stripWhitespace)

#Add Elements to matrix and count number of words
dtm <- TermDocumentMatrix(word_cloud)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#Generate Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(2,0.4))

rm(stop_words, word_cloud, dtm, m, v, d)
gc()

#plot number of comments per day
ggplot(data) + aes(x = Date) + 
  geom_bar() + scale_y_continuous(labels = comma)


#plot sentiment of portfolio with value
# sentiment_portfolio %>%
#   group_by(Month) %>%
#   summarise(mean_sentiment = mean(sentiment, na.rm = T)) %>%
#   ggplot() + geom_line(aes(x = Month, y = mean_sentiment, group = 1))
# 
# 
# 
# #plot sentiment over time
# reddit_sentiment_counts %>% 
#   filter(stock_mention %in% top5) %>% ggplot(aes(x = Date, y = sentiment, color = stock_mention)) +geom_smooth(se = F)
# 
