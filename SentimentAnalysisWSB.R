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
data$created_utc = anytime::utctime(data$created_utc)


as.POSIXct(data$created_utc, origin="1970-01-01")
library(R.utils)

#countLines('wsb_comments_raw.csv')

#create date and time columns 
data = add_column(data, Date = substr(data$created_utc, 1, 10),.before = 1)
data = add_column(data, Time = substr(data$created_utc, 12, 20),.after = 1)

#drop no longer needed column
#data = select(data,-created_utc)

#change date and time formats
data$Date = as.Date(data$Date)
data$Time = times(data$Time)

data = data[order(data$Date, data$Time),]

#reset index after odering 
row.names(data) <- NULL
# Adding Words to Vader Dictionary ----------------------------------------

# let's add some words to the dictionary that are specific to WSB
wsbLexicon <- bind_rows(tibble(V1 = c("retard", "retarded", "fuck", "fucking", "autist", "fag", "gay", "stonk", "porn", 
                                      "degenerate", "boomer", "ape", "gorilla"), V2 = 0, V3 = 0.5), # neutral 
                        tibble(V1 = c("bull", "bullish", "tendie", "tendies", "call", "long", "buy", "moon", "hold",# positive
                                      "diamond", "hands", "yolo", "yoloed", "free", "btfd", "rocket", "elon", "gain",
                                      "420", "calls", "longs", "sky", "space", "roof", "squeeze", "balls", "JPOW", "printer",
                                      "brrr", "HODL", "daddy", "BTFD", "squoze", "full moon", "full moon face", "ox", "astronaut",
                                      "man astronaut", "gem stone", "money bag"), V2 = 1.5, V3 = 0.5),                     
                        tibble(V1 = c("bear", "sell", "put", "short", "shorts", "puts", "bagholder", "wife", "boyfriend",# negative
                                      "shorting", "citron", "hedge", "fake", "virgin", "cuck", "guh", "paper", "SEC", "drilling",
                                      "bear face", "briefcase", "roll of paper"), V2 = -1.5, V3 = 0.5))

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
reddit_mentions <- data %>%
  mutate(stock_mention = str_extract_all(body, reg_expression)) %>%
  unnest(cols = stock_mention)

reddit_mention_counts <- reddit_mentions %>% 
  group_by(Date, stock_mention) %>% 
  count()

X = group_by(reddit_mentions$Date,reddit_mentions$stock_mention)


# false positives (non-stock related):
for (i in LETTERS){
  print(stock_tickers%>%filter(stock_tickers$Symbol == i))
  }

fp <- c("RH", "DD", "CEO", "IMO", "EV", "PM", "TD", "ALL", "USA", "IT", "EOD", "ATH", LETTERS)

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
reddit_mention_counts %>% 
  filter(stock_mention %in% top5) %>% 
  ggplot(aes(x = Date, y = n, color = stock_mention)) + geom_line() #+ theme_classic()


reddit_mentions %>% 
  filter(!(stock_mention %in% fp)) %>% 
  group_by(stock_mention) %>% 
  count() %>% 
  arrange(-n) %>% 
  print(n = 20)

reddit_mentions %>% 
  filter(!(stock_mention %in% fp))

comments_sentiment = reddit_mentions %>%
  select(body) %>%
  distinct() %>%
  mutate(comment_clean = str_replace_all(body, "\\\\", " ")) %>%
  mutate(sentiment = vader_df(comment_clean)$compound)

reddit_mentions_sentiment <- reddit_mentions %>% 
  left_join(comments_sentiment %>% select(-comment_clean),
            by = "body")

reddit_sentiment_counts <- reddit_mentions_sentiment %>% 
  group_by(Date, stock_mention) %>% 
  summarise(sentiment = mean(sentiment),
            n = n())

reddit_sentiment_counts %>% 
  filter(stock_mention %in% top5) %>% 
  ggplot(aes(x = Date, y = sentiment, color = stock_mention)) +
  geom_smooth(se = F) +
  theme_classic()


