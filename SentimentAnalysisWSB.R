#Jonas Schmitten & Noah Angara
#Big Data Analytics
#May 2021

#packages
library('data.table')

#setting working directory
Paths = c("/Users/jonasschmitten/Desktop/FS 2021/Big Data Analytics/Sentiment Analysis WSB", 
          "/Users/noahangara/Documents/Master's/8th Semester/Economics in Practice")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

#partially reading in data 
data = fread('wsb_comments_raw.csv', nrows = 10000)


