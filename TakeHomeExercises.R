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
Paths = c("/Users/jonasschmitten/Desktop/GitHub/BigDataWSB",
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
options(timeout = 1000)

#download zip files
#RUNTIME : ~9 minutes (depends on network speed)
download.file(zip_links, basename(zip_links), method="libcurl")
#unzip and remove zip files
lapply(basename(zip_links), function(x)unzip(x, exdir = getwd()))
file.remove(c(basename(zip_links), "README"))
gc()

#combine csv files into one (only reading one file into memory at a time)
#RUNTIME: ~7.83 minutes
beginning <- Sys.time()
files <- list.files(pattern = ".csv$")
for (i in files) {
  d <- vroom(i, num_threads = detectCores())
  gc()
  first <- i == list.files(pattern = ".csv")[1]
  fwrite(d, "fec.csv", nThread = detectCores(), append = !first)
}
ending <- Sys.time()

#compress csv file and remove redundant files
#RUNTIME: ~3.5 minutes
gzip("fec.csv", destname = "fec.csv.zip")
file.remove(list.files(pattern = "contributions"))


# Task 2 ------------------------------------------------------------------
library(RSQLite)

#Create in-memory SQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")


