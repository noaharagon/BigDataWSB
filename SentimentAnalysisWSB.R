#Jonas Schmitten & Noah Angara
#Big Data Analytics
#May 2021

#packages
library(data.table)
library(dplyr)
library(anytime)
library(tibble)

#setting working directory
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Big Data Analytics/Sentiment Analysis WSB", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

#partially reading in data 
data = as.data.frame(fread('wsb_comments_raw.csv', nrows = 10000))

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

#create date and time columns 
data = add_column(data, Date = substr(data$created_utc, 1, 10),.before = 1)
data = add_column(data, Time = substr(data$created_utc, 12, 20),.after = 1)

#drop no longer needed column
data = select(data,-created_utc)

#Not as date type yet
typeof(data$Date)




