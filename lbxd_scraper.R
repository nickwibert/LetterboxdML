#### Letterboxd Web-Scraper ####
###  Author: Nick Wibert     ###

## Description: As my final project for STA 4241 - Statistical Learning in R,
## I am using some personal movie-diary data that I have been collecting
## since May 2016 using Letterboxd.com.
##
## For each movie logged, I have a 5-star rating as my response,
## as well as covariates (title, watch date, year of release).
## This web-scraping script uses Letterboxd.com to obtain more
## covariates by visiting the URLs that are logged in the original data set
## and pulling additional info (cast/crew, runtime, genre, language, etc.)
## to be used in model fitting / training.

library(rvest)

# project directory
project_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Coursework/STA4241 - Statistical Learning in R/Final Project"

# load in Letterboxd diary
diary <- read.csv(file.path(project_dir, "letterboxd-data", "diary.csv"))

# remove irrelevant column "Date"
diary <- diary[,-1]

# "Tags" column is a comma-separated list; replace it with unique identifiers
# that indicate how I watched the movie (theatre, streaming, blu-ray, dvd)
diary$Tags[grepl("theatre", diary$Tags)] <- "theatre"
diary$Tags[grepl("streaming", diary$Tags)] <- "streaming"
diary$Tags[grepl("blu-ray", diary$Tags)] <- "blu-ray"
diary$Tags[grepl("dvd", diary$Tags)] <- "dvd"
diary$Tags[!grepl("theatre|streaming|blu-ray|dvd", diary$Tags)] <- ""

# load in "watched" (list of all films I've watched with links to their webpages)
films <- read.csv(file.path(project_dir, "letterboxd-data", "watched.csv"))

# initialize vectors for new attributes
runtime <- director <- writer <- editor <- cinematographer <- composer <- c()
producer <- lead <- genre <- studio <- country <- language <- average <- c()

# loop through all the URLs and store new attributes for each film
for(url in films$Letterboxd.URI)
{
  lbxd <- read_html(url)
  
  average <- c(average, as.numeric(substring(html_attr(html_nodes(lbxd,
               xpath='//*[contains(@name, "data2")]'), "content"), 1, 4)))
  
  footer <- html_text(lbxd %>% html_elements(".text-footer"))
  runtime <- c(runtime,
               as.numeric(regmatches(footer, gregexpr("[[:digit:]]+", footer))))  
  
  director <- c(director, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"director")]'))[1])
  
  writer <- c(writer, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"writer")]'))[1])
  
  editor <- c(editor, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"editor")]'))[1])
  
  cinematographer <- c(cinematographer, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"cinematography")]'))[1])
  
  composer <- c(composer, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"composer")]'))[1])
  
  producer <- c(producer, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"producer")]'))[1])
  
  lead <- c(lead, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"actor")]'))[1])
  
  genre <- c(genre, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"genre")]'))[1])
  
  studio <- c(studio, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"studio")]'))[1])
  
  country <- c(country, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"country")]'))[1])
  
  language <- c(language, html_text(html_nodes(lbxd,
                xpath='//p/a[contains(@href,"language")]'))[1])
  
  print(length(average))
}

films$Average.Rating <- average
films$Runtime <- runtime
films$Genre <- genre
films$Director <- director
films$Actor <- lead
films$Writer <- writer
films$Editor <- editor
films$Cinematographer <- cinematographer
films$Composer <- composer
films$Producer <- producer
films$Studio <- studio
films$Country <- country
films$Language <- language


# use SQL to join data frames, adding all the newly-scraped attributes
# to the "diary" dataset
library(sqldf)

## Final data set
data <- sqldf("SELECT * 
               FROM diary
               JOIN films USING(Name,Year)")


# remove unnecessary columns + repeats
data <- data[,-c(3,5,8,9)]

head(data)
# Response: Rating

# Name column won't be used in any analysis, just to connect other details
# to the film in question.

# 16 covariates. Most of these are likely irrelevant; will decide which ones
# to include/exclude during exploratory data analysis. My guesses for
# significant covariates are Avg. Rating, Release Year, Runtime, and Genre.

# save data set to a .csv file
write.csv(data,
          file.path(project_dir, "full-diary.csv"), row.names = FALSE)

