# Load RMySQL package
library(DBI)
library(stringr)
library(tm)
library(tm.plugin.webmining)
library(NLP)
library(openNLP)

# Connect to the database
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "come_stay",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "password")

# set encoding for connection
dbGetQuery(con, 'SET NAMES utf8')

# Create the data frame properties
properties <- dbGetQuery(con, "SELECT * FROM `real_estate` limit 10")

print(properties)

# Disconnect from the database
dbDisconnect(con)

pipe <- function (arg, vectorFunc) {
  Reduce(function (x, f) { f(x) }, vectorFunc, init = arg)
}

stripTag <- function (text) {
  gsub('<[^<>]+?>', ' ', text)
}

cleanDescription <- function (description) {
  pipe(description, 
       c(stripTag, stripWhiteSpace, trimws))
}