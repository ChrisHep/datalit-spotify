# Data Literacy Data Science Project
# Winter term 2021/22

# Analysis of Spotify song features over the course of time
# Dataset from https://www.kaggle.com/rodolfofigueroa/spotify-12m-songs

rm(list=ls())

# set working directory
setwd("C:/Users/chris/Desktop/Data Literacy/Spotify Project")

# load data
data <- read.csv("tracks_features.csv", header = T)
data$duration_sec <- round(data$duration_ms/1000)
data$year <- as.factor(data$year)
head(data)
summary(data)

plot(data$duration_ms, type = "h")

hist(data$duration_ms, density = F)

median(data$duration_ms)
mean(data$duration_ms)

# Analysis: duration of old songs vs new songs

# aggregate mean duration per year ----------------------------------------
durPerYear <- aggregate(x = data$duration_sec,
                        by = list(year = data$year),
                        FUN = mean)
# cut off year 0
durPerYear <- durPerYear[durPerYear$year > 0, ]

plot(durPerYear$year, durPerYear$x)
barplot(height = durPerYear$x)


# aggregate mean duration per decade --------------------------------------
getDecade <- function(x){
  x <- x - x %% 10
  x
}
data$decade <- getDecade(data$year)
durPerDecade <- aggregate(x = data$duration_sec,
                          by = list(decade = data$decade),
                          FUN = mean)
durPerDecade <- durPerDecade[durPerDecade$decade > 0,]
barplot(height = durPerDecade$x,
        names.arg = durPerDecade$decade,
        sub = "Decade")

anovaDecade <- aov(formula = duration_sec ~ year, data = data)
