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

# Task 1 ------------------------------------------------------------------
setwd("/Users/noahangara/Documents/GitHub/take-home-exercises-team-send-it/data")

#get HTML links 
fec_page <- read_html("https://sunlightlabs.github.io/datacommons/bulk_data.html")
zip_links <- fec_page %>% #get all links for the zip files
  html_nodes(xpath="//*[@id='content']/ul[1]") %>% html_nodes("a") %>%
  html_attr("href")

#increase timeout option to download files (default is 60)
getOption("timeout")
options(timeout = 1000)

#download zip files
#RUNTIME : (depends on network speed)
download.file(zip_links, basename(zip_links), method="libcurl")

#unzip and remove zip fies
sapply(basename(zip_links), function(x)unzip(x, exdir = getwd()))
file.remove(c(basename(zip_links), "README"))
gc()

#combine csv files into one (only reading one file into memory at a time)
#RUNTIME: 
for (i in list.files(pattern = ".csv")) {
  d <- vroom(i, num_threads = 4)
  gc()
  first <- i == list.files(pattern = ".csv")[1]
  vroom_write(d, "fec.csv", num_threads = 4, append = !first)
}

#compress csv file and remove redundant files
zip(zipfile = "fec.csv.zip", files = "fec.csv")
file.remove(list.files(pattern = ".csv$"))

