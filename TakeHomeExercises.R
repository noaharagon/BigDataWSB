#Big Data Analytics
#June 2021
#Jonas Schmitten and Noah Angara


#required libraries
library(xml2)
library(httr)
library(rvest)
library(dplyr)
library(readr)
library(vroom)
library(parallel)
library(R.utils)
library(data.table)

# Task 1 ------------------------------------------------------------------
# set working directory according to who is executing the code
Paths = c("/Users/jonasschmitten/Downloads/GitHub/take-home-exercises-team-send-it/data",
          "/Users/noahangara/Documents/GitHub/take-home-exercises-team-send-it/data")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

#get HTML links 
fec_page <- read_html("https://sunlightlabs.github.io/datacommons/bulk_data.html")
zip_links <- fec_page %>% #get all links for the zip files by contribution year
  html_nodes(xpath="//*[@id='content']/ul[1]") %>% html_nodes("a") %>%
  html_attr("href")

#increase timeout option to download files (default is 60)
getOption("timeout")
options(timeout = 2000)

#download zip files
#RUNTIME : ~9 minutes (depends on network speed) on Macbook Air 2017 i7 8GB RAM
download.file(zip_links, basename(zip_links), method="libcurl")
#unzip and remove zip files
lapply(basename(zip_links), function(x)unzip(x, exdir = getwd()))
file.remove(c(basename(zip_links), "README"))
gc()

#combine csv files into one (only reading one file into memory at a time)
#RUNTIME: ~7 minutes on Macbook Air 2017 i7 8GB RAM & 4 cores
beginning <- Sys.time()
files <- list.files(pattern = "contribution")
for (i in files) {
  d <- vroom(i, num_threads = detectCores(), na = c("", "NA", " "))
  gc()
  first <- i == list.files(pattern = "contribution")[1]
  fwrite(d, "fec.csv", nThread = detectCores(), sep = "~", append = !first, na = NA)
}
ending <- Sys.time()

#compress csv file and remove redundant files
#RUNTIME: ~3.5 minutes on Macbook Air 2017 i7 8GB RAM
gzip("fec.csv", destname = "fec.csv.zip")
file.remove(list.files(pattern = "contributions"))


# Task 2 ------------------------------------------------------------------

#required libraries
library(RSQLite)
library(htmltab)
library(R.utils)

# set working directory according to who is executing the code
Paths = c("/Users/jonasschmitten/Downloads/GitHub/take-home-exercises-team-send-it/data",
          "/Users/noahangara/Documents/GitHub/take-home-exercises-team-send-it/data")
names(Paths) = c("jonasschmitten", "noahangara")
setwd(Paths[Sys.info()[7]])

#Download additional data
transactions <- htmltab(doc = "https://www.fec.gov/campaign-finance-data/transaction-type-code-descriptions/"
                        , which = '//*[@id="main"]/article/div/div/div/div[2]/div/table')
colnames(transactions) <- c("Type", "Description")

industrycodes <- read.csv("http://assets.transparencydata.org.s3.amazonaws.com/docs/catcodes.csv")

#unzip fec.csv
R.utils:::gunzip("fec.csv.zip", destname = "fec.csv")

#Create in-memory SQLite database
con <- dbConnect(RSQLite::SQLite(), "fec.sqlite")

#Create table for transaction types
dbWriteTable(con, "transactiontypes", transactions, field.types = c(
  Type = "varchar(3)",
  Description = "text"))

#Create table for industry 
dbWriteTable(con, "industrycodes", industrycodes, field.types = c(
  source = "varchar(3)",
  code = "varchar(5)",
  name = "varchar(96)",
  industry = "varchar(50)",
  order = "varchar(3)"))

#Create index for those variables needed in later analysis (makes retrieval more efficient)
#For transaction type data

dbExecute(con, 'CREATE INDEX index_transaction ON transactiontypes (Type,Description);')
#For industry code data

dbExecute(con, 'CREATE INDEX index_industry ON industrycodes (source, code, name, industry);')

#Create table for donations"
dbWriteTable(con, name = "donations", value = "fec.csv", sep = "~")
